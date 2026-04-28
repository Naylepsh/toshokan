package assetScraping.downloading

import assetScraping.downloading.domain.AssetEntryDir
import cats.effect.IO
import cats.mtl.Handle
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
      Handle
        .allow[DownloadError]:
          service
            .download(entryId)
            .flatMap: entryDir =>
              Ok(
                view.renderDownloadResult(entryDir),
                `Content-Type`(MediaType.text.html)
              )
        .rescue(handleError)

    case POST -> Root / "bulk" / AssetIdVar(assetId) =>
      Handle
        .allow[DownloadError]:
          service
            .downloadAll(assetId)
            .flatMap: progress =>
              Ok(
                view.renderBulkDownloadResult(progress),
                `Content-Type`(MediaType.text.html)
              )
        .rescue(handleError)

  private def handleError(error: DownloadError): IO[Response[IO]] =
    error match
      case DownloadError.AssetNotFound(id) => NotFound(s"Asset not found: $id")
      case DownloadError.NoAssetFoundForEntry(id) =>
        NotFound(s"No asset found for entry: $id")
      case DownloadError.AssetHasNoEntry(aId, eId) =>
        NotFound(s"Asset $aId has no entry $eId")
      case DownloadError.UnsupportedUrl(url) =>
        BadRequest(s"Unsupported url for downloading: $url")
      case DownloadError.NoEntriesApplicableForDownload =>
        BadRequest("No entries applicable for download")

  val routes = Router("asset-downloading" -> httpRoutes)
