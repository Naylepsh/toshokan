package myAnimeList

import cats.data.NonEmptyList
import cats.effect.*
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import doobie.ConnectionIO
import doobie.implicits.*
import doobie.util.transactor.Transactor
import doobie.{util as _, *}
import sttp.model.Uri

import util.control.NoStackTrace
import db.extensions.*
import domain.*
import schemas.{Manga as _, *}

case object NoAuthToken extends NoStackTrace
type NoAuthToken = NoAuthToken.type

case object NoCodeChallenge extends NoStackTrace
type NoCodeChallenge = NoCodeChallenge.type

trait MyAnimeListService:
  def searchForManga(term: Term): IO[List[Manga]]
  def updateProgress(malId: ExternalMangaId, chapter: LatestChapter): IO[Unit]
  def acquireToken(code: String): Raise[IO, NoCodeChallenge] ?=> IO[Unit]
  def prepareForTokenAcquisition: IO[Uri]

object MyAnimeListService:
  def make(
      xa: Transactor[IO],
      malClient: Option[MyAnimeListClient]
  ): IO[MyAnimeListService] =
    malClient.fold(IO.pure(MyAnimeListServiceNoop())): client =>
      MyAnimeListServiceImpl.make(xa, client).widen

class MyAnimeListServiceNoop extends MyAnimeListService:
  override def searchForManga(term: Term): IO[List[Manga]] =
    IO.pure(List.empty)
  override def updateProgress(malId: ExternalMangaId, chapter: LatestChapter): IO[Unit] =
    IO.unit
  override def prepareForTokenAcquisition: IO[Uri] =
    IO.pure(Uri("http://localhost:8080/doesnt-matter"))
  override def acquireToken(code: String): Raise[IO, NoCodeChallenge] ?=> IO[Unit] =
    IO.unit

class MyAnimeListServiceImpl(
    malClient: MyAnimeListClient,
    tokenManager: TokenManager,
    codeChallengeRef: Ref[IO, Option[String]]
) extends MyAnimeListService:
  override def searchForManga(term: Term): IO[List[Manga]] =
    tokenManager.withToken: token =>
      term match
        case Term.Id(id) =>
          malClient
            .find(token, id)
            .map:
              case None        => List.empty
              case Some(manga) => List(Manga(ExternalMangaId(manga.id), MangaTitle(manga.title)))
        case name @ Term.Name(_) =>
          malClient
            .searchManga(token, name)
            .map: body =>
              body.data.map: mangaData =>
                Manga(ExternalMangaId(mangaData.node.id), MangaTitle(mangaData.node.title))

  override def updateProgress(malId: ExternalMangaId, chapter: LatestChapter): IO[Unit] =
    tokenManager.withToken(malClient.updateStatus(_, malId, chapter))

  override val prepareForTokenAcquisition: IO[Uri] =
    for
      codeChallenge <- malClient.generateCodeChallenge
      authLink = malClient.createAuthorizationLink(codeChallenge)
      _ <- codeChallengeRef.set(codeChallenge.some)
    yield authLink

  override def acquireToken(code: String): Raise[IO, NoCodeChallenge] ?=> IO[Unit] =
    codeChallengeRef.get.flatMap(_.fold(NoCodeChallenge.raise): codeChallenge =>
      malClient
        .acquireToken(code, codeChallenge)
        .flatMap(tokenManager.saveToken)
        .flatTap(_ => codeChallengeRef.set(None))
        .void)

object MyAnimeListServiceImpl:
  def make(xa: Transactor[IO], malClient: MyAnimeListClient): IO[MyAnimeListServiceImpl] =
    for
      tokenRef      <- Ref.of[IO, Option[AuthToken]](None)
      codeChallenge <- Ref.of[IO, Option[String]](None)
      tokenManager = TokenManager(malClient, tokenRef, xa)
    yield MyAnimeListServiceImpl(malClient, tokenManager, codeChallenge)

private class TokenManager(
    malClient: MyAnimeListClient,
    tokenRef: Ref[IO, Option[AuthToken]],
    xa: Transactor[IO]
):
  def withToken[A](f: AuthToken => IO[A]): IO[A] =
    tokenRef.get
      .flatMap(_.fold(getOrRefreshToken)(_.some.pure))
      .flatMap(_.fold(NoAuthToken.raiseError[IO, A])(f))

  def saveToken(token: AuthToken): IO[Unit] =
    IO.realTime.flatMap: now =>
      val nowMillis              = now.toMillis
      val refreshTokenExpiresAt  = nowMillis + (token.expiresIn * 1000) * 3
      val accessTokenExpiresAt   = nowMillis + (token.expiresIn * 1000)
      val saveAccessToken  = TokensSql.upsertToken("mal-access-token", token.accessToken, accessTokenExpiresAt)
      val saveRefreshToken = TokensSql.upsertToken("mal-refresh-token", token.refreshToken, refreshTokenExpiresAt)
      (saveAccessToken *> saveRefreshToken).transact(xa)

  private val getOrRefreshToken: IO[Option[AuthToken]] =
    getToken
      .flatMap:
        case (Some(accessToken), Some(refreshToken)) =>
          AuthToken(0L, refreshToken, accessToken).some.pure
        case (None, Some(refreshToken)) =>
          scribe.cats[IO].info("Access token expired, refreshing...")
            *> malClient.refreshAuthToken(refreshToken).flatTap(saveToken).map(_.some)
        case _ => IO.pure(None)
      .flatMap: token =>
        tokenRef.set(token).as(token)

  private def getToken: IO[(Option[AccessToken], Option[RefreshToken])] =
    for
      access <- TokensSql.getToken("mal-access-token").transact(xa).map(_.map(AccessToken(_)))
      refresh <- TokensSql.getToken("mal-refresh-token").transact(xa).map(_.map(RefreshToken(_)))
    yield (access, refresh)

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

  def upsertToken(name: String, value: String, expiresAt: Long): ConnectionIO[Unit] =
    sql"""
    ${Tokens.insertIntoX(
        NonEmptyList.of(
          _.name_ --> name,
          _.value --> value,
          _.expiresAt --> expiresAt
        )
      )}
    ON CONFLICT (${Tokens.name_})
    DO UPDATE SET ${Tokens.value === value}, ${Tokens.expiresAt === expiresAt}
    """.update.run.void
