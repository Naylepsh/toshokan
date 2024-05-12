package scraper.sites.mangadex

import cats.effect.MonadCancelThrow
import cats.implicits.*
import sttp.capabilities.WebSockets
import sttp.client3.circe.*
import sttp.client3.{SttpBackend, UriContext, basicRequest}

trait MangadexApi[F[_]]:
  def getMangaFeed(mangaId: String): F[Either[Throwable, GetMangaFeedResponse]]

object MangadexApi:
  def make[F[_]: MonadCancelThrow](
      backend: SttpBackend[F, WebSockets]
  ): MangadexApi[F] = new:
    def getMangaFeed(
        mangaId: String
    ): F[Either[Throwable, GetMangaFeedResponse]] =
      val url =
        uri"https://api.mangadex.org/manga/$mangaId/feed?order[chapter]=desc&translatedLanguage[]=en"

      basicRequest
        .get(url)
        .response(asJson[GetMangaFeedResponse])
        .send(backend)
        .map(_.body)
        .handleError(_.asLeft)
