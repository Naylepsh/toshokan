package mangadex

import java.net.URI

import cats.effect.MonadCancelThrow
import cats.implicits.*
import mangadex.schemas.manga.GetMangaResponse
import sttp.capabilities.WebSockets
import sttp.client3.circe.*
import sttp.client3.{SttpBackend, UriContext, basicRequest}

import schemas.*

trait MangadexApi[F[_]]:
  def getMangaFeed(
      mangaId: String
  ): F[Either[Throwable, feed.GetMangaFeedResponse]]
  def getManga(
      mangaId: String
  ): F[Either[Throwable, manga.GetMangaResponse]]
  def getImages(chapterId: String): F[Either[Throwable, List[URI]]]

object MangadexApi:
  def make[F[_]: MonadCancelThrow](
      backend: SttpBackend[F, WebSockets]
  ): MangadexApi[F] = new:
    override def getMangaFeed(
        mangaId: String
    ): F[Either[Throwable, feed.GetMangaFeedResponse]] =
      val url =
        uri"https://api.mangadex.org/manga/$mangaId/feed?order[chapter]=desc&translatedLanguage[]=en&contentRating[]=safe&contentRating[]=suggestive&contentRating[]=erotica&contentRating[]=pornographic"

      basicRequest
        .get(url)
        .response(asJson[feed.GetMangaFeedResponse])
        .send(backend)
        .map(_.body)
        .handleError(_.asLeft)

    override def getManga(
        mangaId: String
    ): F[Either[Throwable, GetMangaResponse]] =
      val url =
        uri"https://api.mangadex.org/manga/$mangaId"

      basicRequest
        .get(url)
        .response(asJson[manga.GetMangaResponse])
        .send(backend)
        .map(_.body)
        .handleError(_.asLeft)

    override def getImages(chapterId: String): F[Either[Throwable, List[URI]]] =
      val url = uri"https://api.mangadex.org/at-home/server/${chapterId}"

      basicRequest
        .get(url)
        .response(asJson[server.GetChapterFilesResponse])
        .send(backend)
        .map(_.body.map(_.chapter.urls))
        .handleError(_.asLeft)
