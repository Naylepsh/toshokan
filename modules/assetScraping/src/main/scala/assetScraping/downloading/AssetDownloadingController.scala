package assetScraping.downloading

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import library.AssetController.EntryIdVar
import org.http4s.*
import org.http4s.headers.*
import org.http4s.server.Router

class AssetDownloadingController[F[_]: MonadCancelThrow](
    service: AssetDownloadingService[F],
    view: AssetDownloadingView
) extends http.Controller[F]:
  import http.Controller.given

  private val httpRoutes = HttpRoutes.of[F]:
    case POST -> Root / EntryIdVar(entryId) =>
      service
        .download(entryId)
        .flatMap: entryDir =>
          Ok(
            view.renderDownloadResult(entryDir),
            `Content-Type`(MediaType.text.html)
          )

  val routes = Router("asset-downloading" -> httpRoutes)
