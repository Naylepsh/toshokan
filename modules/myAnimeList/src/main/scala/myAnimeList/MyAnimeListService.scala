package myAnimeList

import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*
import cats.{Functor, Parallel}
import doobie.ConnectionIO
import doobie.implicits.*
import doobie.util.fragment.Fragment
import doobie.util.transactor.Transactor
import doobiex.*
import sttp.model.Uri

import util.control.NoStackTrace
import domain.*
import schemas.{Manga as _, *}
import cats.Applicative

case object NoAuthToken extends NoStackTrace
type NoAuthToken = NoAuthToken.type

case object NoCodeChallenge extends NoStackTrace
type NoCodeChallenge = NoCodeChallenge.type

private type Result[A] = Either[Throwable, A]

trait MyAnimeListService[F[_]]:
  def searchForManga(term: Term): F[Either[Throwable, List[Manga]]]
  def updateProgress(
      malId: ExternalMangaId,
      chapter: LatestChapter
  ): F[Either[Throwable, Unit]]
  def acquireToken(code: String): F[Either[Throwable, Unit]]
  def prepareForTokenAcquisition: F[Uri]

object MyAnimeListService:
  def make[F[_]: Sync: Parallel](
      xa: Transactor[F],
      malClient: Option[MyAnimeListClient[F]]
  ): F[MyAnimeListService[F]] =
    malClient.fold(new MyAnimeListServiceNoop().pure): client =>
      /** `identity`'s here so that `MyAnimeListServiceImpl[F]` can be converted
        * to `MyAnimeListService[F]`
        */
      MyAnimeListServiceImpl.make[F](xa, client).map(identity)

class MyAnimeListServiceNoop[F[_]: Applicative] extends MyAnimeListService[F]:
  override def searchForManga(term: Term): F[Either[Throwable, List[Manga]]] =
    List.empty.asRight.pure
  override def updateProgress(
      malId: ExternalMangaId,
      chapter: LatestChapter
  ): F[Either[Throwable, Unit]] = Either.unit.pure
  override def prepareForTokenAcquisition: F[Uri] = Uri(
    "http://localhost:8080/doesnt-matter"
  ).pure
  override def acquireToken(code: String): F[Either[Throwable, Unit]] =
    Either.unit.pure

class MyAnimeListServiceImpl[F[_]: Sync: Parallel](
    xa: Transactor[F],
    malClient: MyAnimeListClient[F],
    tokenRef: Ref[F, Option[AuthToken]],
    codeChallengeRef: Ref[F, Option[String]]
) extends MyAnimeListService[F]:
  override def searchForManga(
      term: Term
  ): F[Either[Throwable, List[Manga]]] =
    withToken: token =>
      term match
        case Term.Id(id) =>
          Functor[F]
            .compose[Result]
            .map(malClient.find(token, id)):
              case None => List.empty
              case Some(manga) =>
                List(
                  Manga(ExternalMangaId(manga.id), MangaTitle(manga.title))
                )
        case name @ Term.Name(_) =>
          Functor[F]
            .compose[Result]
            .map(malClient.searchManga(token, name)): body =>
              body.data.map: mangaData =>
                Manga(
                  ExternalMangaId(mangaData.node.id),
                  MangaTitle(mangaData.node.title)
                )

  override def updateProgress(
      malId: ExternalMangaId,
      chapter: LatestChapter
  ): F[Either[Throwable, Unit]] =
    withToken(malClient.updateStatus(_, malId, chapter))

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
          .flatMap(_.traverse(saveToken))
          .flatTap(_ => codeChallengeRef.set(None))
    )

  private def withToken[A](f: AuthToken => F[Either[Throwable, A]]) =
    tokenRef.get
      .flatMap(_.fold(getOrRefreshToken)(_.some.pure))
      .flatMap(_.fold(NoAuthToken.asLeft.pure)(f))

  private val getOrRefreshToken: F[Option[AuthToken]] =
    getToken
      .flatMap:
        case (Some(accessToken), Some(refreshToken)) =>
          AuthToken(0L, refreshToken, accessToken).some.pure
        case (None, Some(refreshToken)) =>
          updateToken(refreshToken)
        case _ => None.pure
      .flatMap: token =>
        tokenRef.set(token).as(token)

  private def getToken: F[(Option[AccessToken], Option[RefreshToken])] =
    for
      access <- TokensSql
        .getToken("mal-access-token")
        .transact(xa)
        .map(_.map(AccessToken(_)))
      refresh <-
        TokensSql
          .getToken("mal-refresh-token")
          .transact(xa)
          .map(_.map(RefreshToken(_)))
    yield (access, refresh)

  private def updateToken(token: RefreshToken): F[Option[AuthToken]] =
    malClient
      .refreshAuthToken(token)
      .flatMap:
        case Left(error) =>
          scribe.cats[F].error(error.getMessage).as(None)
        case Right(token) =>
          saveToken(token).as(token.some)

  private def saveToken(token: AuthToken): F[Unit] =
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

object MyAnimeListServiceImpl:
  def make[F[_]: Sync: Parallel](
      xa: Transactor[F],
      malClient: MyAnimeListClient[F]
  ): F[MyAnimeListServiceImpl[F]] =
    for
      token         <- Ref.of[F, Option[AuthToken]](None)
      codeChallenge <- Ref.of[F, Option[String]](None)
    yield MyAnimeListServiceImpl[F](xa, malClient, token, codeChallenge)

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
