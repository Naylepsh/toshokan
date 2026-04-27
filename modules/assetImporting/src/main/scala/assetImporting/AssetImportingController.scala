package assetImporting

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Decoder
import neotype.interop.circe.given
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router
import org.typelevel.ci.CIString

import domain.MangadexMangaUri

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
        service
          .importFromMangadex(mangadexManga.uri)
          .flatMap: asset =>
            Response[IO]()
              .withStatus(Status.Created)
              .withHeaders(
                Header.Raw(CIString("HX-Location"), s"/assets/${asset.id}")
              )
              .pure

  val routes = Router("asset-importing" -> httpRoutes)

object AssetImportingController:
  case class MangadexManga(uri: MangadexMangaUri) derives Decoder

  given EntityDecoder[IO, MangadexManga] = jsonOf[IO, MangadexManga]
