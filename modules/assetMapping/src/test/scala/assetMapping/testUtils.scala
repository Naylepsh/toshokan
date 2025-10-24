package assetMapping
package testUtils

import cats.effect.IO
import myAnimeList.*
import myAnimeList.domain.*
import myAnimeList.schemas.{Manga, *}
import sttp.model.Uri

val noopMalClient: MyAnimeListClient[IO] = new:
  override def generateCodeChallenge: IO[String] = IO.pure("CH4LL3NG3")

  override def createAuthorizationLink(codeChallenge: String): Uri = ???

  override def refreshAuthToken(
      token: RefreshToken
  ): IO[AuthToken] = IO.pure:
    AuthToken(
      0L,
      RefreshToken("r3fr3sh-tok3n"),
      AccessToken("4cc355-t0k3n")
    )

  override def acquireToken(
      code: String,
      codeChallenge: String
  ): IO[AuthToken] = IO.pure:
    AuthToken(
      0L,
      RefreshToken("r3fr3sh-tok3n"),
      AccessToken("4cc355-t0k3n")
    )

  override def searchManga(
      token: AuthToken,
      term: Term.Name
  ): IO[GetMangaListSuccess] = ???

  override def find(
      token: AuthToken,
      mangaId: ExternalMangaId
  ): IO[Option[Manga]] = ???

  override def updateStatus(
      token: AuthToken,
      mangaId: ExternalMangaId,
      latestChapter: LatestChapter
  ): IO[Unit] = ???
