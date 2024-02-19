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
    service: AssetService[F]
) extends Http4sDsl[F]:
  import AssetController.{ *, given }

  private val htmlContentTypeHeader = `Content-Type`(MediaType.text.html)

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      service.findAll.flatMap: assetsWithEntries =>
        Ok(AssetView.renderAssets(assetsWithEntries), htmlContentTypeHeader)

    case GET -> Root / "new" =>
      Ok(AssetView.renderForm(None), htmlContentTypeHeader)

    case GET -> Root / "edit" / AssetIdVar(id) =>
      service.find(id).flatMap:
        case Some(asset, _) =>
          Ok(AssetView.renderForm(asset.some), htmlContentTypeHeader)
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

    case GET -> Root / "entries-by-release-date" =>
      service.findAllGroupedByReleaseDate.attempt.flatMap:
        case Left(reason) =>
          println(reason)
          InternalServerError("Something went wrong")
        case Right(releases) =>
          Ok(AssetView.renderReleases(releases), htmlContentTypeHeader)

  val routes = Router("assets" -> httpRoutes)

  // TODO: Move this to a Controller base class?
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

  private def addRedirectHeaderIfHtmxRequest[F[_]](
      request: Request[F],
      redirectTo: String
  ): List[Header.Raw] =
    if request.headers.get(CIString("HX-Request")).isDefined then
      Header.Raw(CIString("HX-Location"), redirectTo) :: Nil
    else Nil
