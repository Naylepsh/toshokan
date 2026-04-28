package assetImporting

import cats.effect.IO
import cats.mtl.Handle
import cats.syntax.all.*
import io.circe.Decoder
import neotype.interop.circe.given
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router
import org.typelevel.ci.CIString

import domain.{ImportError, MangadexMangaUri}

class AssetImportingController(
    service: AssetImportingService,
    view: AssetImportingView
) extends http.Controller:
  import AssetImportingController.{*, given}

  private val httpRoutes = HttpRoutes.of[IO]:
    case GET -> Root =>
      Ok(
        view.renderForm.toString,
        `Content-Type`(MediaType.text.html)
      )

    case req @ POST -> Root / "mangadex" =>
      withJsonErrorsHandled[MangadexManga](req): mangadexManga =>
        Handle
          .allow[ImportError]:
            service
              .importFromMangadex(mangadexManga.uri)
              .flatMap: asset =>
                Response[IO]()
                  .withStatus(Status.Created)
                  .withHeaders(
                    Header.Raw(CIString("HX-Location"), s"/assets/${asset.id}")
                  )
                  .pure
          .rescue:
            case ImportError.CategoryDoesNotExist =>
              BadRequest("Manga category does not exist")
            case ImportError.AssetAlreadyExists =>
              Conflict("Asset already exists")
            case ImportError.NoTitleTranslation =>
              BadRequest("No title translation available")
            case ImportError.ScrapingConfigError(msg) =>
              BadRequest(s"Scraping config error: $msg")
            case ImportError.MappingError(msg) =>
              BadRequest(s"Mapping error: $msg")

  val routes = Router("asset-importing" -> httpRoutes)

object AssetImportingController:
  case class MangadexManga(uri: MangadexMangaUri) derives Decoder

  given EntityDecoder[IO, MangadexManga] = jsonOf[IO, MangadexManga]
