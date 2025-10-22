package assetImporting

import cats.effect.{Concurrent, MonadCancelThrow}
import cats.syntax.all.*
import io.circe.Decoder
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router
import org.typelevel.ci.CIString

import domain.MangadexMangaUri

class AssetImportingController[F[_]: MonadCancelThrow: Concurrent](
    service: AssetImportingService[F],
    view: AssetImportingView
) extends http.Controller[F]:
  import AssetImportingController.*

  private val httpRoutes = HttpRoutes.of[F]:
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
            // TODO: Handle domain errors?
            Response[F]()
              .withStatus(Status.Created)
              .withHeaders(
                Header.Raw(CIString("HX-Location"), s"/assets/${asset.id}")
              )
              .pure

  val routes = Router("asset-importing" -> httpRoutes)

object AssetImportingController:
  case class MangadexManga(uri: MangadexMangaUri) derives Decoder

  given [F[_]: Concurrent]: EntityDecoder[F, MangadexManga] =
    jsonOf[F, MangadexManga]
