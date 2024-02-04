package library

import cats.effect.{ Concurrent, IO, MonadCancelThrow }
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import io.github.arainko.ducktape.*
import library.domain.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.*
import org.http4s.server.Router
import org.typelevel.ci.CIString

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
        .as[NewAssetPayload]
        .attempt
        .flatMap:
          case Left(InvalidMessageBodyFailure(details, cause)) =>
            BadRequest(cause.map(_.toString).getOrElse(details))
          case Left(error) =>
            println(s"[ERROR]: $error")
            InternalServerError("Something went wrong")
          case Right(newAsset) =>
            service.add(newAsset.to[NewAsset], newAsset.configs).flatMap:
              case Left(AddAssetError.AssetAlreadyExists) =>
                Conflict(s"${newAsset.title} already exists")
              case Left(AddScrapingConfigError.ConfigAlreadyExists) =>
                Conflict(
                  s"At least one of the configs for ${newAsset.title} already exists"
                )
              case Left(AddScrapingConfigError.AssetDoesNotExists) =>
                InternalServerError("Asset disappeared")
              case Right(asset, configs) =>
                Ok(asset.id.value.toString, addRedirectHeaderIfHtmxRequest(req))

    case req @ PUT -> Root / AssetIdVar(assetId) =>
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
            val asset = newAsset.asExisting(assetId)
            service.update(asset).flatMap: _ =>
              Ok(asset.id.value.toString, addRedirectHeaderIfHtmxRequest(req))

    case DELETE -> Root / AssetIdVar(assetId) =>
      service.delete(assetId) *> Ok()

  val routes = Router("assets" -> httpRoutes)

object AssetController:
  object AssetIdVar:
    def unapply(str: String): Option[AssetId] =
      str.toIntOption.map(AssetId(_))

  case class NewAssetPayload(
      title: AssetTitle,
      configs: List[NewScrapingConfig]
  ) derives Decoder

  given [F[_]: Concurrent]: EntityDecoder[F, NewAsset] = jsonOf[F, NewAsset]
  given [F[_]: Concurrent]: EntityDecoder[F, NewAssetPayload] =
    jsonOf[F, NewAssetPayload]

  private def addRedirectHeaderIfHtmxRequest[F[_]](request: Request[F])
      : List[Header.Raw] =
    if request.headers.get(CIString("HX-Request")).isDefined then
      Header.Raw(CIString("HX-Location"), "/assets") :: Nil
    else Nil
