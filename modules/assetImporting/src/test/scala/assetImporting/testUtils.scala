package assetImporting
package testUtils

import java.net.URI

import cats.effect.IO
import cats.syntax.all.*
import mangadex.MangadexApi
import mangadex.schemas.*
import mangadex.schemas.manga.*

val stubMangadexApi: MangadexApi = new:
  override def getManga(
      mangaId: String
  ): IO[Either[Throwable, GetMangaResponse]] =
    mangaId match
      case "abc-123" =>
        GetMangaResponse(
          Manga(
            MangaAttributes(
              Map("en" -> "Test Manga"),
              MangaLinks(Some("12345"))
            ),
            Some(
              List(
                Relationship(
                  "author",
                  Some(RelationshipAttributes(Some("Author One")))
                ),
                Relationship(
                  "artist",
                  Some(RelationshipAttributes(Some("Artist Two")))
                )
              )
            )
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

  override def getMangaFeed(
      mangaId: String
  ): IO[Either[Throwable, feed.GetMangaFeedResponse]] = ???
  override def getImages(chapterId: String): IO[Either[Throwable, List[URI]]] =
    ???
