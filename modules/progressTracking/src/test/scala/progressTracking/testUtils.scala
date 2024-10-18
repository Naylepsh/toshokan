package progressTracking
package testUtils

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.util.transactor.Transactor
import sttp.model.Uri

import domain.*
import mal.*
import progressTracking.domain.Term.Name
import db.transactors.makeSqliteTransactorResource
import db.{Config, Path}

val noopMalClient: MyAnimeListClient[IO] = new:
  override def generateCodeChallenge: IO[String] = IO.pure("CH4LL3NG3")
  override def createAuthorizationLink(codeChallenge: String): Uri = ???
  override def refreshAuthToken(
      token: RefreshToken
  ): IO[Either[Throwable, AuthToken]] = IO.pure(
    Right(
      AuthToken(
        0L,
        RefreshToken("r3fr3sh-tok3n"),
        AccessToken("4cc355-t0k3n")
      )
    )
  )
  override def acquireToken(
      code: String,
      codeChallenge: String
  ): IO[Either[Throwable, AuthToken]] = IO.pure(
    Right(
      AuthToken(
        0L,
        RefreshToken("r3fr3sh-tok3n"),
        AccessToken("4cc355-t0k3n")
      )
    )
  )
  override def searchManga(
      token: AuthToken,
      term: Name
  ): IO[Either[Throwable, GetMangaListSuccess]] = ???
  override def find(
      token: AuthToken,
      mangaId: ExternalMangaId
  ): IO[Either[Throwable, Option[mal.Manga]]] = ???
  override def updateStatus(
      token: AuthToken,
      mangaId: ExternalMangaId,
      latestChapter: LatestChapter
  ): IO[Either[Throwable, Unit]] = ???

val inMemoryTransactor =
  makeSqliteTransactorResource[IO](Config.forSqlite(Path("sqlite::memory:")))

val migrationSQL = IO:
  new String(
    Files.readAllBytes(Paths.get("./db/schema.sql")),
    StandardCharsets.UTF_8
  )

def applyMigrations(xa: Transactor[IO]): IO[Unit] =
  migrationSQL.flatMap: sql =>
    sql
      .split(";")
      .toList
      .filterNot(_.isBlank)
      .traverse: statement =>
        Fragment.const(statement).update.run
      .transact(xa)
      .void
