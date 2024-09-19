package progressTracking

import cats.data.{EitherT, NonEmptyList, OptionT}
import cats.effect.*
import cats.syntax.all.*
import cats.{Functor, Parallel}
import doobie.ConnectionIO
import doobie.implicits.*
import doobie.util.fragment.Fragment
import doobie.util.query.Query0
import doobie.util.transactor.Transactor
import doobiex.*
import library.AssetService
import library.category.CategoryService
import library.domain.*
import progressTracking.mal.{Manga as _, *}
import sttp.model.Uri

import util.chaining.*
import util.control.NoStackTrace
import domain.*
import core.Tuples

case object NoAuthToken extends NoStackTrace
type NoAuthToken = NoAuthToken.type

case object NoCodeChallenge extends NoStackTrace
type NoCodeChallenge = NoCodeChallenge.type

case object AssetNotFound extends NoStackTrace
type AssetNotFound = AssetNotFound.type

case object CategoryNotFound extends NoStackTrace
type CategoryNotFound = CategoryNotFound.type

case object AssetIsNotManga extends NoStackTrace
type AssetIsNotManga = AssetIsNotManga.type

case object ExternalIdAlreadyInUse extends NoStackTrace
type ExternalIdAlreadyInUse = ExternalIdAlreadyInUse.type

case object MangaAlreadyHasExternalIdAssigned extends NoStackTrace
type MangaAlreadyHasExternalIdAssigned = MangaAlreadyHasExternalIdAssigned.type

type AssignExternalIdToMangaError = AssetNotFound | CategoryNotFound |
  AssetIsNotManga | ExternalIdAlreadyInUse | MangaAlreadyHasExternalIdAssigned

type FindMalMappingError = AssetNotFound | CategoryNotFound | AssetIsNotManga
/*
 * ProgressTracking module should probably sit on top of the library module?
 * Then the `wasSeen` should be probably moved from there to this module as well?
 * Thus this module needs a controller for handling these updates as well
 */

trait ProgressTrackingService[F[_]]:
  def searchForManga(term: Term): F[Either[Throwable, List[Manga]]]
  def findAssetWithMalMapping(
      assetId: AssetId
  ): F[Either[FindMalMappingError, Option[
    (ExistingAsset, ExistingMalMangaMapping)
  ]]]
  def deleteMapping(id: AssetId): F[Unit]
  def assignExternalIdToManga(
      externalId: ExternalMangaId,
      internalId: AssetId
  ): F[Either[AssignExternalIdToMangaError, Unit]]
  def updateProgress(
      assetId: AssetId,
      entryId: EntryId,
      wasEntrySeen: WasEntrySeen
  ): F[Either[UpdateEntryError, (ExistingAsset, ExistingAssetEntry)]]
  def acquireToken(code: String): F[Either[Throwable, Unit]]
  def prepareForTokenAcquisition: F[Uri]
  def findNotSeenReleases: F[List[Releases]]

private type Result[A] = Either[Throwable, A]

object ProgressTrackingService:
  def make[F[_]: Sync: Parallel](
      xa: Transactor[F],
      malClient: MyAnimeListClient[F],
      assetService: AssetService[F],
      categoryService: CategoryService[F]
  ): F[ProgressTrackingService[F]] =
    for
      token         <- Ref.of[F, Option[AuthToken]](None)
      codeChallenge <- Ref.of[F, Option[String]](None)
    yield make(
      xa,
      malClient,
      assetService,
      categoryService,
      token,
      codeChallenge
    )

  def make[F[_]: Sync: Parallel](
      xa: Transactor[F],
      malClient: MyAnimeListClient[F],
      assetService: AssetService[F],
      categoryService: CategoryService[F],
      tokenRef: Ref[F, Option[AuthToken]],
      codeChallengeRef: Ref[F, Option[String]]
  ): ProgressTrackingService[F] = new:
    override def searchForManga(
        term: Term
    ): F[Either[Throwable, List[Manga]]] =
      withToken: token =>
        Functor[F]
          .compose[Result]
          .map(malClient.searchManga(token, term)): body =>
            body.data.map: mangaData =>
              Manga(
                ExternalMangaId(mangaData.node.id),
                MangaTitle(mangaData.node.title)
              )

    override def findAssetWithMalMapping(
        assetId: AssetId
    ): F[Either[FindMalMappingError, Option[
      (ExistingAsset, ExistingMalMangaMapping)
    ]]] =
      (for
        (asset, _) <- EitherT.fromOptionF(
          assetService.find(assetId),
          AssetNotFound: FindMalMappingError
        )
        categoryId <- EitherT.fromOption(asset.categoryId, CategoryNotFound)
        category <- EitherT.fromOptionF(
          categoryService.find(categoryId),
          CategoryNotFound
        )
        mangaId <- EitherT.fromOption(
          MangaId(asset.id, category.name),
          AssetIsNotManga
        )
        mapping <- EitherT.liftF(MalMangaSql.findMapping(mangaId).transact(xa))
      yield mapping.map(asset -> _)).value

    override def deleteMapping(assetId: AssetId): F[Unit] =
      (for
        (asset, _) <- OptionT(assetService.find(assetId))
        categoryId <- OptionT.fromOption(asset.categoryId)
        category   <- OptionT(categoryService.find(categoryId))
        mangaId    <- OptionT.fromOption(MangaId(assetId, category.name))
        _          <- OptionT.liftF(MalMangaSql.delete(mangaId).transact(xa))
      yield ()).value.void

    override def assignExternalIdToManga(
        externalId: ExternalMangaId,
        internalId: AssetId
    ): F[Either[AssignExternalIdToMangaError, Unit]] =
      val getMangaId = for
        (asset, _) <- EitherT.fromOptionF(
          assetService.find(internalId),
          AssetNotFound: AssignExternalIdToMangaError
        )
        category <- EitherT.fromOptionF(
          asset.categoryId.traverse(categoryService.find).map(_.flatten),
          CategoryNotFound
        )
        mangaId <- EitherT.fromOption(
          MangaId(asset.id, category.name),
          AssetIsNotManga
        )
      yield mangaId

      getMangaId.value.flatMap:
        case Left(error) => error.asLeft.pure
        case Right(mangaId) =>
          (
            MalMangaSql.findMalId(mangaId),
            MalMangaSql.findMangaId(externalId)
          ).tupled
            .transact(xa)
            .flatMap:
              case (_, Some(_)) =>
                ExternalIdAlreadyInUse.asLeft.pure
              case (Some(_), _) =>
                MangaAlreadyHasExternalIdAssigned.asLeft.pure
              case (None, None) =>
                MalMangaSql
                  .assignMalIdToManga(externalId, mangaId)
                  .transact(xa)
                  .map(_.asRight)

    override def updateProgress(
        assetId: AssetId,
        entryId: EntryId,
        wasEntrySeen: WasEntrySeen
    ): F[Either[UpdateEntryError, (ExistingAsset, ExistingAssetEntry)]] =
      assetService
        .find(assetId)
        .flatMap:
          case None => UpdateEntryError.AssetDoesNotExists.asLeft.pure
          case Some(asset, entries) =>
            (
              // TODO: entry management (setSeen) should be moved to progress tracking?
              // Probably as a separate database entity (separate from entry)
              assetService.setSeen(assetId, entryId, wasEntrySeen),
              if wasEntrySeen
              then
                entries
                  .find(_.id.eqv(entryId))
                  .fold(Sync[F].unit): entry =>
                    updateProgressOnMal(asset, entries, entry.no).flatMap:
                      result =>
                        result.fold(
                          error => scribe.cats[F].error(error.toString),
                          _ => Sync[F].unit
                        )
              else Sync[F].unit
            ).mapN((a, _) => a)

    override val prepareForTokenAcquisition: F[Uri] =
      for
        codeChallenge <- malClient.generateCodeChallenge
        authLink = malClient.createAuthorizationLink(codeChallenge)
        _ <- codeChallengeRef.set(codeChallenge.some)
      yield authLink

    override def acquireToken(code: String): F[Either[Throwable, Unit]] =
      codeChallengeRef.get.flatMap(_.fold(NoCodeChallenge.asLeft.pure):
        codeChallenge =>
          malClient
            .acquireToken(code, codeChallenge)
            .flatMap(_.traverse(saveMalToken))
            .flatTap(_ => codeChallengeRef.set(None))
      )

    override def findNotSeenReleases: F[List[Releases]] =
      // TODO: Move the AssetService's logic here and remove this method from it?
      assetService.findNotSeenReleases

    private def withToken[A](f: AuthToken => F[Either[Throwable, A]]) =
      tokenRef.get
        .flatMap(_.fold(getOrRefreshToken)(_.some.pure))
        .flatMap(_.fold(NoAuthToken.asLeft.pure)(f))

    private val getOrRefreshToken: F[Option[AuthToken]] =
      getMalToken
        .flatMap:
          case (Some(accessToken), Some(refreshToken)) =>
            AuthToken(0L, refreshToken, accessToken).some.pure
          case (None, Some(refreshToken)) =>
            refreshMalToken(refreshToken)
          case _ => None.pure
        .flatMap: token =>
          tokenRef.set(token).as(token)

    private def getMalToken: F[(Option[AccessToken], Option[RefreshToken])] =
      (
        TokensSql
          .getToken("mal-access-token")
          .transact(xa)
          .map(_.map(AccessToken(_))),
        TokensSql
          .getToken("mal-refresh-token")
          .transact(xa)
          .map(_.map(RefreshToken(_)))
      ).parTupled

    private def refreshMalToken(token: RefreshToken): F[Option[AuthToken]] =
      malClient
        .refreshAuthToken(token)
        .flatMap:
          case Left(error) =>
            scribe.cats[F].error(error.getMessage).as(None)
          case Right(token) =>
            saveMalToken(token).as(token.some)

    private def saveMalToken(token: AuthToken): F[Unit] =
      Clock[F].realTime.flatMap: now =>
        val nowMillis = now.toMillis

        /** I have no clue what's the actual expiration date for refresh tokens,
          * so I kinda eyeball it
          */
        val refreshTokenExpiresAt = nowMillis + (token.expiresIn * 1000) * 3
        val accessTokenExpiresAt  = nowMillis + (token.expiresIn * 1000)

        val saveAccessToken = TokensSql.upsertToken(
          "mal-access-token",
          token.accessToken,
          accessTokenExpiresAt
        )
        val saveRefreshToken = TokensSql.upsertToken(
          "mal-refresh-token",
          token.refreshToken,
          refreshTokenExpiresAt
        )

        (saveAccessToken *> saveRefreshToken).transact(xa)

    private def updateProgressOnMal(
        asset: ExistingAsset,
        entries: List[ExistingAssetEntry],
        no: EntryNo
    ): F[Either[Throwable, Unit]] =
      val getMalId = for
        category <- asset.categoryId
          .traverse(categoryService.find)
          .map(_.flatten)
          .pipe(OptionT(_))
        mangaId <- Option
          .when(isLatestEntry(no, entries))(MangaId(asset.id, category.name))
          .flatten
          .pipe(OptionT.fromOption(_))
        malId <- OptionT(MalMangaSql.findMalId(mangaId).transact(xa))
      yield malId

      getMalId.value
        .map((_, LatestChapter(no)).tupled)
        .flatMap(_.fold(Either.unit.pure): (malId, latestChapter) =>
          withToken(malClient.updateStatus(_, malId, latestChapter)))

  /** EntryNo can be just a number, but it can also be a full chapter title. It
    * would probably make sense to force a split into separate EntryTitle and
    * EntryNo (which could default to 1 in the case of extraction failure)
    *
    * Any EntryNo with fractional number should not be considered latest, as we
    * don't known whether there are any more entries within the same integer
    * mark
    */
  def isLatestEntry(
      no: EntryNo,
      allEntries: List[ExistingAssetEntry]
  ): Boolean =
    no.value.toIntOption
      .map: noNumeric =>
        allEntries
          .mapFilter(_.no.value.toIntOption)
          .pipe(NonEmptyList.fromList)
          .fold(false)(_.maximum == noNumeric)
      .getOrElse(false)

private object MalMangaMappings extends TableDefinition("mal_manga_mapping"):
  val id      = Column[MalMangaMappingId]("id")
  val mangaId = Column[MangaId]("manga_id")
  val malId   = Column[ExternalMangaId]("mal_id")

  val * = Columns(id, mangaId, malId)

private object MalMangaSql:
  def assignMalIdToManga(
      externalId: ExternalMangaId,
      internalId: MangaId
  ): ConnectionIO[Unit] =
    insertInto(
      MalMangaMappings,
      NonEmptyList.of(_.mangaId --> internalId, _.malId --> externalId)
    ).update.run.void

  def findMalId(
      mangaId: MangaId
  ): ConnectionIO[Option[ExternalMangaId]] =
    sql"""
    SELECT ${MalMangaMappings.malId}
    FROM ${MalMangaMappings}
    WHERE ${MalMangaMappings.mangaId === mangaId}"""
      .queryOf(MalMangaMappings.malId)
      .option

  def findMangaId(malId: ExternalMangaId): ConnectionIO[Option[MangaId]] =
    sql"""
    SELECT ${MalMangaMappings.mangaId}
    FROM ${MalMangaMappings}
    WHERE ${MalMangaMappings.malId === malId}"""
      .queryOf(MalMangaMappings.mangaId)
      .option

  def findMapping(
      mangaId: MangaId
  ): ConnectionIO[Option[ExistingMalMangaMapping]] =
    sql"""
    SELECT ${MalMangaMappings.*}
    FROM ${MalMangaMappings}
    WHERE ${MalMangaMappings.mangaId === mangaId}
    """
      .queryOf(MalMangaMappings.*)
      .option
      .map(_.map(Tuples.from[ExistingMalMangaMapping](_)))

  def delete(mangaId: MangaId) =
    sql"""
    DELETE FROM ${MalMangaMappings}
    WHERE ${MalMangaMappings.mangaId === mangaId}
    """.update.run.void

private object Tokens extends TableDefinition("tokens"):
  val id        = Column[Long]("id")
  val name_     = Column[String]("name")
  val value     = Column[String]("value")
  val expiresAt = Column[Long]("expires_at")

private object TokensSql:
  def getToken(name: String): ConnectionIO[Option[String]] =
    sql"""
    SELECT ${Tokens.value}
    FROM ${Tokens}
    WHERE ${Tokens.name_ === name}
    AND ${Tokens.expiresAt} > (strftime('%s', 'now') * 1000)"""
      .query[String]
      .option

  def upsertToken(
      name: String,
      value: String,
      expiresAt: Long
  ): ConnectionIO[Unit] =
    sql"""
    ${insertInto(
        Tokens,
        NonEmptyList.of(
          _.name_ --> name,
          _.value --> value,
          _.expiresAt --> expiresAt
        )
      )}
    ON CONFLICT (${Tokens.name_}) DO UPDATE SET ${Tokens.value === value}
    """.update.run.void
