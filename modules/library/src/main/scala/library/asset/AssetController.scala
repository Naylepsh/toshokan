package library.asset

import cats.effect.IO
import cats.implicits.*
import cats.mtl.Handle
import io.circe.Decoder
import library.asset.domain.*
import library.category.CategoryService
import library.category.domain.CategoryId
import neotype.*
import neotype.interop.circe.given
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router
import org.typelevel.ci.CIString

class AssetController(
    assetService: AssetService,
    categoryService: CategoryService,
    view: AssetView
) extends http.Controller:
  import http.Controller.given
  import AssetController.{*, given}

  private val httpRoutes = HttpRoutes.of[IO]:
    case GET -> Root =>
      assetService.findAll
        .map(_.sortBy(_._1.title))
        .flatMap: assetsWithEntries =>
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
      withJsonErrorsHandled[NewAssetDto](req): newAsset =>
        Handle
          .allow[AddAssetError]:
            assetService
              .add(newAsset.toDomain)
              .flatMap: asset =>
                Ok(
                  asset.id.unwrap.toString,
                  addRedirectHeaderIfHtmxRequest(req, s"/assets/${asset.id}")
                )
          .rescue:
            case AssetAlreadyExists =>
              Conflict(s"${newAsset.title} already exists")

    case req @ PUT -> Root / AssetIdVar(id) =>
      withJsonErrorsHandled[NewAssetDto](req): newAsset =>
        val asset = newAsset.asExisting(id)
        assetService.update(asset) *> Ok(
          asset.id.unwrap.toString,
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

  case class NewAssetDto(
      title: AssetTitle,
      categoryId: Option[CategoryId]
  ) derives Decoder:
    def toDomain: NewAsset = NewAsset(title, categoryId, Nil)
    def asExisting(id: AssetId): ExistingAsset = toDomain.asExisting(id)

  given EntityDecoder[IO, NewAssetDto] = jsonOf[IO, NewAssetDto]

  private def addRedirectHeaderIfHtmxRequest(
      request: Request[IO],
      redirectTo: String
  ): List[Header.Raw] =
    if request.headers.get(CIString("HX-Request")).isDefined then
      Header.Raw(CIString("HX-Location"), redirectTo) :: Nil
    else Nil
