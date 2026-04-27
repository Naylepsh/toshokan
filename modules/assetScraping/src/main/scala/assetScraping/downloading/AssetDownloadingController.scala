package assetScraping.downloading

import assetScraping.downloading.domain.AssetEntryDir
import cats.effect.IO
import library.asset.AssetController.{AssetIdVar, EntryIdVar}
import org.http4s.*
import org.http4s.headers.*
import org.http4s.server.Router

class AssetDownloadingController(
    service: AssetDownloadingService[AssetEntryDir],
    view: AssetDownloadingView
) extends http.Controller:
  import http.Controller.given

  private val httpRoutes = HttpRoutes.of[IO]:
    case POST -> Root / EntryIdVar(entryId) =>
      service
        .download(entryId)
        .flatMap: entryDir =>
          Ok(
            view.renderDownloadResult(entryDir),
            `Content-Type`(MediaType.text.html)
          )

    case POST -> Root / "bulk" / AssetIdVar(assetId) =>
      service
        .downloadAll(assetId)
        .flatMap: progress =>
          Ok(
            view.renderBulkDownloadResult(progress),
            `Content-Type`(MediaType.text.html)
          )

  val routes = Router("asset-downloading" -> httpRoutes)
