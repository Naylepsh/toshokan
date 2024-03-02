package library

import cats.effect.{ Concurrent, MonadCancelThrow }
import cats.syntax.all.*
import library.domain.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router
import org.typelevel.ci.CIString

class AssetController[F[_]: MonadCancelThrow: Concurrent, A](
    service: AssetService[F]
) extends http.Controller[F]:
  import AssetController.{ *, given }

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      service.findAll.flatMap: assetsWithEntries =>
        Ok(
          AssetView.renderAssets(assetsWithEntries),
          `Content-Type`(MediaType.text.html)
        )

    case GET -> Root / "new" =>
      Ok(AssetView.renderForm(None), `Content-Type`(MediaType.text.html))

    case GET -> Root / "edit" / AssetIdVar(id) =>
      service.find(id).flatMap:
        case Some(asset, _) =>
          Ok(
            AssetView.renderForm(asset.some),
            `Content-Type`(MediaType.text.html)
          )
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
          Ok(
            AssetView.renderReleases(releases),
            `Content-Type`(MediaType.text.html)
          )

  val routes = Router("assets" -> httpRoutes)

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
