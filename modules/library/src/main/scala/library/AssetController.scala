package library

import cats.effect.{ Concurrent, MonadCancelThrow }
import cats.syntax.all.*
import io.circe.Decoder
import library.domain.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router
import org.typelevel.ci.CIString

class AssetController[F[_]: MonadCancelThrow: Concurrent](
    service: AssetService[F],
    view: AssetView
) extends http.Controller[F]:
  import AssetController.{ *, given }

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      service.findAll.flatMap: assetsWithEntries =>
        Ok(
          view.renderAssets(assetsWithEntries),
          `Content-Type`(MediaType.text.html)
        )

    case GET -> Root / "new" =>
      Ok(view.renderForm(None), `Content-Type`(MediaType.text.html))

    case GET -> Root / "edit" / AssetIdVar(id) =>
      service.find(id).flatMap:
        case Some(asset, _) =>
          Ok(
            view.renderForm(asset.some),
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

    case req @ PATCH -> Root
        / AssetIdVar(assetId)
        / "entries"
        / EntryIdVar(entryId) =>
      withJsonErrorsHandled[PartialAssetEntry](req): assetEntry =>
        assetEntry
          .wasSeen
          .map: wasSeen =>
            service.setSeen(assetId, entryId, wasSeen).flatMap:
              case Left(reason)        => BadRequest("Oops")
              case Right(asset, entry) => Ok(view.entryPartial(asset, entry))
          .getOrElse(Ok(""))

    case GET -> Root / "entries-by-release-date" =>
      service.findAllGroupedByReleaseDate.attempt.flatMap:
        case Left(reason) =>
          InternalServerError("Something went wrong")
        case Right(releases) =>
          Ok(
            view.renderReleases(releases),
            `Content-Type`(MediaType.text.html)
          )

  val routes = Router("assets" -> httpRoutes)

object AssetController:
  object AssetIdVar:
    def unapply(str: String): Option[AssetId] =
      str.toIntOption.map(AssetId(_))

  object EntryIdVar:
    def unapply(str: String): Option[EntryId] =
      str.toIntOption.map(EntryId(_))

  case class PartialAssetEntry(
      wasSeen: Option[WasEntrySeen]
  ) derives Decoder
  object PartialAssetEntry:
    given [F[_]: Concurrent]: EntityDecoder[F, PartialAssetEntry] =
      jsonOf[F, PartialAssetEntry]

  given [F[_]: Concurrent]: EntityDecoder[F, NewAsset] = jsonOf[F, NewAsset]
  given [F[_]]: EntityEncoder[F, scalatags.Text.TypedTag[String]] =
    EntityEncoder
      .stringEncoder[F]
      .contramap[scalatags.Text.TypedTag[String]](_.render)

  private def addRedirectHeaderIfHtmxRequest[F[_]](
      request: Request[F],
      redirectTo: String
  ): List[Header.Raw] =
    if request.headers.get(CIString("HX-Request")).isDefined then
      Header.Raw(CIString("HX-Location"), redirectTo) :: Nil
    else Nil
