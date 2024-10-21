package library

import cats.effect.{Concurrent, MonadCancelThrow}
import cats.syntax.all.*
import io.circe.Decoder
import library.category.CategoryService
import library.domain.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router
import org.typelevel.ci.CIString

class AssetController[F[_]: MonadCancelThrow: Concurrent](
    assetService: AssetService[F],
    categoryService: CategoryService[F],
    view: AssetView
) extends http.Controller[F]:
  import http.Controller.given
  import AssetController.{*, given}

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      assetService.findAll.flatMap: assetsWithEntries =>
        Ok(
          view.renderAssets(assetsWithEntries),
          `Content-Type`(MediaType.text.html)
        )

    case GET -> Root / "new" =>
      categoryService.findAll.flatMap: categories =>
        Ok(
          view.renderForm(categories),
          `Content-Type`(MediaType.text.html)
        )

    case GET -> Root / AssetIdVar(id) =>
      (assetService.find(id), categoryService.findAll).tupled.flatMap:
        case (None, _) => NotFound(s"Asset ${id} not found")
        case (Some(asset, entries), categories) =>
          Ok(
            view.renderAsset(
              asset,
              entries.sortBy(entry => (entry.no, entry.dateUploaded))(using
                Ordering[(EntryNo, DateUploaded)].reverse
              ),
              categories
            ),
            `Content-Type`(MediaType.text.html)
          )

    case req @ POST -> Root =>
      withJsonErrorsHandled[NewAsset](req): newAsset =>
        assetService
          .add(newAsset)
          .flatMap:
            case Left(AssetAlreadyExists) =>
              Conflict(s"${newAsset.title} already exists")
            case Right(asset) =>
              Ok(
                asset.id.value.toString,
                addRedirectHeaderIfHtmxRequest(
                  req,
                  s"/assets/${asset.id}"
                )
              )

    case req @ PUT -> Root / AssetIdVar(id) =>
      withJsonErrorsHandled[NewAsset](req): newAsset =>
        val asset = newAsset.asExisting(id)
        assetService.update(asset) *> Ok(
          asset.id.value.toString,
          addRedirectHeaderIfHtmxRequest(req, "/assets")
        )

    case DELETE -> Root / AssetIdVar(id) =>
      assetService.delete(id) *> Ok()

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

  private def addRedirectHeaderIfHtmxRequest[F[_]](
      request: Request[F],
      redirectTo: String
  ): List[Header.Raw] =
    if request.headers.get(CIString("HX-Request")).isDefined then
      Header.Raw(CIString("HX-Location"), redirectTo) :: Nil
    else Nil
