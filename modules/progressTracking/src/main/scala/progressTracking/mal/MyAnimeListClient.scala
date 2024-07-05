package progressTracking
package mal

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Ref
import cats.syntax.all.*
import sttp.capabilities.WebSockets
import sttp.client3.circe.*
import sttp.client3.{SttpBackend, UriContext, basicRequest}

import domain.{MangaId, LatestChapter, Term}

trait MyAnimeListClient[F[_]]:
  def searchManga(term: Term): F[Either[Throwable, GetMangaListSuccess]]
  def updateStatus(
      mangaId: MangaId,
      latestChapter: LatestChapter
  ): F[Either[Throwable, Unit]]
  def acquireToken(): F[AuthToken]
  def refreshToken(): F[AuthToken]

object MyAnimeListClient:
  def make[F[_]: MonadCancelThrow](
      backend: SttpBackend[F, WebSockets],
      token: Ref[F, AuthToken]
  ): MyAnimeListClient[F] = new:
    override def searchManga(
        term: Term
    ): F[Either[Throwable, GetMangaListSuccess]] =
      val url = uri"https://api.myanimelist.net/v2/manga?q=$term"

      token.get.flatMap: token =>
        basicRequest
          .get(url)
          .auth
          .bearer(token.accessToken)
          .response(asJson[GetMangaListSuccess])
          .send(backend)
          .map(_.body)
          .handleError(_.asLeft)

    override def updateStatus(
        mangaId: MangaId,
        latestChapter: LatestChapter
    ): F[Either[Throwable, Unit]] =
      val url =
        uri"https://api.myanimelist.net/v2/manga/$mangaId/my_list_status"

      token.get.flatMap: token =>
        basicRequest
          .patch(url)
          .body(UpdateMangaStatusBody(MangaStatus.Reading, latestChapter))
          .auth
          .bearer(token.accessToken)
          .send(backend)
          .map: response =>
            response.body.void.leftMap(new RuntimeException(_))

    override def acquireToken(): F[AuthToken] = ???
    override def refreshToken(): F[AuthToken] = ???
