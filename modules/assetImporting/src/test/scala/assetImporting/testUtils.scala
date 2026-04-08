package assetImporting
package testUtils

import java.net.URI

import cats.effect.IO
import cats.syntax.all.*
import mangadex.MangadexApi
import mangadex.schemas.*
import mangadex.schemas.manga.*
import myAnimeList.*
import myAnimeList.domain.{ExternalMangaId, LatestChapter, Term}
import myAnimeList.schemas.{AuthToken, RefreshToken}
import sttp.model.Uri

val noopMalClient: MyAnimeListClient[IO] = new:
  override def generateCodeChallenge: IO[String]                                                                = IO.pure("CH4LL3NG3")
  override def createAuthorizationLink(codeChallenge: String): Uri                                              = ???
  override def refreshAuthToken(token: RefreshToken): IO[AuthToken]                                             = ???
  override def acquireToken(code: String, codeChallenge: String): IO[AuthToken]                                 = ???
  override def searchManga(token: AuthToken, term: Term.Name): IO[myAnimeList.schemas.GetMangaListSuccess]      = ???
  override def find(token: AuthToken, mangaId: ExternalMangaId): IO[Option[myAnimeList.schemas.Manga]]          = ???
  override def updateStatus(token: AuthToken, mangaId: ExternalMangaId, latestChapter: LatestChapter): IO[Unit] = ???

val stubMangadexApi: MangadexApi[IO] = new:
  override def getManga(mangaId: String): IO[Either[Throwable, GetMangaResponse]] =
    mangaId match
      case "abc-123" =>
        GetMangaResponse(
          Manga(
            MangaAttributes(
              Map("en" -> "Test Manga"),
              MangaLinks(Some("12345"))
            ),
            Some(List(
              Relationship("author", Some(RelationshipAttributes(Some("Author One")))),
              Relationship("artist", Some(RelationshipAttributes(Some("Artist Two"))))
            ))
          )
        ).asRight.pure
      case "no-authors" =>
        GetMangaResponse(
          Manga(
            MangaAttributes(
              Map("en" -> "No Author Manga"),
              MangaLinks(None)
            ),
            None
          )
        ).asRight.pure
      case other =>
        RuntimeException(s"Unknown manga: $other").asLeft.pure

  override def getMangaFeed(mangaId: String): IO[Either[Throwable, feed.GetMangaFeedResponse]] = ???
  override def getImages(chapterId: String): IO[Either[Throwable, List[URI]]]                  = ???
