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

    case GET -> Root / "edit" / AssetIdVar(id) =>
      service.find(id).flatMap:
        case Some(asset, _) =>
          Ok(view.renderForm(asset.some), `Content-Type`(view.mediaType))
        case None =>
          NotFound(s"Asset ${id} not found")

    case req @ POST -> Root =>
      withJsonErrorsHandled[NewAsset](req): newAsset =>
        service.add(newAsset).flatMap:
          case Left(AddAssetError.AssetAlreadyExists) =>
            Conflict(s"${newAsset.title} already exists")
          case Right(asset) =>
            Ok(
              asset.id.value.toString,
              addRedirectHeaderIfHtmxRequest(
                req,
                s"assets/edit/${asset.id}"
              )
            )

    case req @ PUT -> Root / AssetIdVar(id) =>
      withJsonErrorsHandled[NewAsset](req): newAsset =>
        val asset = newAsset.asExisting(id)
        service.update(asset) *> Ok(
          asset.id.value.toString,
          addRedirectHeaderIfHtmxRequest(req, "/assets")
        )

    case DELETE -> Root / AssetIdVar(id) =>
      service.delete(id) *> Ok()

    case GET -> Root / "assets" / "entries-by-release-date" =>
      service.findAllGroupedByReleaseDate.flatMap: results =>
        ???

  val routes = Router("assets" -> httpRoutes)

  private def withJsonErrorsHandled[A](request: Request[F])(using
  EntityDecoder[F, A]): (A => F[Response[F]]) => F[Response[F]] = f =>
    request.as[A].attempt.flatMap:
      case Left(InvalidMessageBodyFailure(details, cause)) =>
        BadRequest(cause.map(_.toString).getOrElse(details))
      case Left(error) =>
        println(s"[ERROR]: $error")
        InternalServerError("Something went wrong")
      case Right(a) => f(a)

object AssetController:
  object AssetIdVar:
    def unapply(str: String): Option[AssetId] =
      str.toIntOption.map(AssetId(_))

  given [F[_]: Concurrent]: EntityDecoder[F, NewAsset] = jsonOf[F, NewAsset]
  given [F[_]: Concurrent]: EntityDecoder[F, NewAssetScrapingConfig] =
    jsonOf[F, NewAssetScrapingConfig]

  private def addRedirectHeaderIfHtmxRequest[F[_]](
      request: Request[F],
      redirectTo: String
  ): List[Header.Raw] =
    if request.headers.get(CIString("HX-Request")).isDefined then
      Header.Raw(CIString("HX-Location"), redirectTo) :: Nil
    else Nil
