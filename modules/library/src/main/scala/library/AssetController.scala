package library

import cats.effect.{ Concurrent, IO, MonadCancelThrow }
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import library.domain.{ AssetId, NewAsset }
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.*
import org.http4s.server.Router

class AssetController[F[_]: MonadCancelThrow: Concurrent, A](
    service: AssetService[F],
    view: AssetView[F, A]
)(using EntityEncoder[F, A]) extends Http4sDsl[F]:
  import AssetController.{ *, given }

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      service.findAll.flatMap: assetsWithEntries =>
        Ok(view.renderAssets(assetsWithEntries), `Content-Type`(view.mediaType))

    case GET -> Root / "new" =>
      Ok(view.renderForm(None), `Content-Type`(view.mediaType))

    case GET -> Root / "edit" / AssetIdVar(assetId) =>
      service.find(assetId).flatMap:
        case Some(asset, _) =>
          Ok(view.renderForm(asset.some), `Content-Type`(view.mediaType))
        case None =>
          NotFound(s"Asset ${assetId} not found")

    case req @ POST -> Root =>
      req
        .as[NewAsset]
        .attempt
        .flatMap:
          case Left(InvalidMessageBodyFailure(details, cause)) =>
            BadRequest(cause.map(_.toString).getOrElse(details))
          case Left(error) =>
            println(s"[ERROR]: $error")
            InternalServerError("Something went wrong")
          case Right(newAsset) =>
            Ok(s"Asset {newAsset} will be created shortly... (TODO)")

  val routes = Router("assets" -> httpRoutes)

object AssetController:
  object AssetIdVar:
    def unapply(str: String): Option[AssetId] =
      str.toIntOption.map(AssetId(_))

  given [F[_]: Concurrent]: EntityDecoder[F, NewAsset] = jsonOf[F, NewAsset]
