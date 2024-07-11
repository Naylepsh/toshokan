package progressTracking

import cats.data.{NonEmptyList, OptionT}
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
import library.domain.{AssetId, EntryNo, ExistingAssetEntry}
import progressTracking.mal.{Manga as _, *}

import util.chaining.*
import domain.*

/*
 * ProgressTracking module should probably sit on top of the library module?
 * Then the `wasSeen` should be probably moved from there to this module as well?
 * Thus this module needs a controller for handling these updates as well
 */

trait ProgressTrackingService[F[_]]:
  def searchForManga(term: Term): F[Either[Throwable, List[Manga]]]
  def assignExternalIdToManga(
      externalId: ExternalMangaId,
      internalId: MangaId
  ): F[Unit]
  def updateProgress(assetId: AssetId, no: EntryNo): F[Either[Throwable, Unit]]

type EitherThrowable[A] = Either[Throwable, A]

object ProgressTrackingService:
  def make[F[_]: Sync: Parallel](
      xa: Transactor[F],
      malClient: MyAnimeListClient[F],
      assetService: AssetService[F],
      categoryService: CategoryService[F]
  ): F[ProgressTrackingService[F]] =
    Ref
      .of[F, Option[AuthToken]](None)
      .map(make(xa, malClient, assetService, categoryService, _))

  def make[F[_]: Sync: Parallel](
      xa: Transactor[F],
      malClient: MyAnimeListClient[F],
      assetService: AssetService[F],
      categoryService: CategoryService[F],
      tokenRef: Ref[F, Option[AuthToken]]
  ): ProgressTrackingService[F] = new:
    override def searchForManga(
        term: Term
    ): F[Either[Throwable, List[Manga]]] =
      withToken: token =>
        Functor[F]
          .compose[EitherThrowable]
          .map(malClient.searchManga(token, term)): body =>
            body.data.map: d =>
              Manga(ExternalMangaId(d.node.id), MangaTitle(d.node.title))

    override def assignExternalIdToManga(
        externalId: ExternalMangaId,
        internalId: MangaId
    ): F[Unit] = MalMangaSql
      .assignMalIdToManga(externalId, internalId)
      .transact(xa)

    override def updateProgress(
        assetId: AssetId,
        no: EntryNo
    ): F[Either[Throwable, Unit]] =
      withToken: token =>
        val getMangaId = for
          (asset, entries) <- OptionT(assetService.find(assetId))
          category <- asset.categoryId
            .traverse(categoryService.find)
            .map(_.flatten)
            .pipe(OptionT(_))
          mangaId <- Option
            .when(isLatestEntry(no, entries))(MangaId(asset.id, category.name))
            .flatten
            .pipe(OptionT.fromOption(_))
        yield mangaId

        getMangaId.value
          .map((_, LatestChapter(no)).tupled)
          .flatMap(
            _.fold(Either.unit.pure)(malClient.updateStatus(token, _, _))
          )

    private def withToken[A](f: AuthToken => F[Either[Throwable, A]]) =
      tokenRef.get
        .flatMap(_.fold(getOrRefreshToken)(_.some.pure))
        .flatMap(
          _.fold(new RuntimeException("No auth token set").asLeft.pure)(f)
        )

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
      Clock[F].monotonic.flatMap: now =>
        val nowMillis = now.toMillis

        /** I have no clue what's the actual expiration date for refresh tokens,
          * so I kinda eyeball it
          */
        val refreshTokenExpiresAt = nowMillis + token.expiresIn * 3
        val accessTokenExpiresAt  = nowMillis + token.expiresIn

        val program = for
          _ <- TokensSql.upsertToken(
            "mal-access-token",
            token.accessToken,
            accessTokenExpiresAt
          )
          _ <- TokensSql.upsertToken(
            "mal-refresh-token",
            token.refreshToken,
            refreshTokenExpiresAt
          )
        yield ()
        program.transact(xa)

  /** EntryNo can be just a number, but it can also be a full chapter title. It
    * would probably make sense to force a split into separate EntryTitle and
    * EntryNo (which could default to 1 in the case of extraction failure)
    */
  def isLatestEntry(
      no: EntryNo,
      allEntries: List[ExistingAssetEntry]
  ): Boolean =
    no.value.toDoubleOption
      .map: noNumeric =>
        allEntries
          .mapFilter(_.no.value.toDoubleOption)
          .pipe(NonEmptyList.fromList)
          .fold(true)(_.maximum < noNumeric)
      .getOrElse(false)

private object MalMangaMapping extends TableDefinition("mal_manga_mapping"):
  val id      = Column[Long]("id")
  val mangaId = Column[MangaId]("manga_id")
  val malId   = Column[ExternalMangaId]("mal_id")

private object MalMangaSql:
  def assignMalIdToManga(
      externalId: ExternalMangaId,
      internalId: MangaId
  ): ConnectionIO[Unit] =
    insertInto(
      MalMangaMapping,
      NonEmptyList.of(_.mangaId --> internalId, _.malId --> externalId)
    ).update.run.void

  def findMalId(
      mangaId: MangaId
  ): ConnectionIO[Option[ExternalMangaId.Type]] =
    sql"""
    SELECT ${MalMangaMapping.malId}
    FROM ${MalMangaMapping}
    WHERE ${MalMangaMapping.mangaId === mangaId}"""
      .queryOf(MalMangaMapping.malId)
      .option

private object Tokens extends TableDefinition("tokens"):
  val id        = Column[Long]("id")
  val name_     = Column[String]("name")
  val value     = Column[String]("value")
  val expiresAt = Column[Long]("expires_at")

private object TokensSql:
  def getToken(name: String): ConnectionIO[Option[String]] =
    sql"""
    SELECT ${Tokens.name_}
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
    ${insertInto(Tokens, NonEmptyList.of(_.name_ --> name, _.value --> value))}
    ON CONFLICT ${Tokens.name_} DO UPDATE SET ${Tokens.value === value}
    """.update.run.void
