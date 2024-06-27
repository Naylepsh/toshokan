package library

import cats.effect.{Concurrent, MonadCancelThrow}
import cats.syntax.all.*
import io.circe.Decoder
import library.category.CategoryService
import library.domain.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.impl.OptionalQueryParamDecoderMatcher
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
          view.renderForm(None, categories),
          `Content-Type`(MediaType.text.html)
        )

    case GET -> Root / "edit" / AssetIdVar(id) =>
      (assetService.find(id), categoryService.findAll).tupled.flatMap:
        case (None, _) => NotFound(s"Asset ${id} not found")
        case (Some(asset, _), categories) =>
          Ok(
            view.renderForm(asset.some, categories),
            `Content-Type`(MediaType.text.html)
          )

    case req @ POST -> Root =>
      withJsonErrorsHandled[NewAsset](req): newAsset =>
        assetService
          .add(newAsset)
          .flatMap:
            case Left(AddAssetError.AssetAlreadyExists) =>
              Conflict(s"${newAsset.title} already exists")
            case Right(asset) =>
              Ok(
                asset.id.value.toString,
                addRedirectHeaderIfHtmxRequest(
                  req,
                  s"/assets/edit/${asset.id}"
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

    case req @ PATCH -> Root
        / AssetIdVar(assetId)
        / "entries"
        / EntryIdVar(entryId) =>
      withJsonErrorsHandled[PartialAssetEntry](req): assetEntry =>
        assetEntry.wasSeen
          .map: wasSeen =>
            assetService
              .setSeen(assetId, entryId, wasSeen)
              .flatMap:
                case Left(reason)        => BadRequest("Oops")
                case Right(asset, entry) => Ok(view.entryPartial(asset, entry))
          .getOrElse(Ok(""))

    case GET -> Root / "entries-by-release-date" =>
      assetService.findNotSeenReleases.attempt.flatMap:
        case Left(reason) =>
          InternalServerError("Something went wrong")
        case Right(releases) =>
          val (items, pagination) = Pagination.paginate(releases, page = 1)
          Ok(
            view.renderReleases(items, pagination),
            `Content-Type`(MediaType.text.html)
          )

    case GET -> Root / "partials" / "entries-by-release-date" :? OptionalPageQueryParam(
          page
        ) =>
      assetService.findNotSeenReleases.attempt.flatMap:
        case Left(reason) =>
          InternalServerError("Something went wrong")
        case Right(releases) =>
          val (items, pagination) =
            Pagination.paginate(releases, page.getOrElse(1))
          Ok(
            view.releasesPartial(items, pagination).toString,
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

  object OptionalPageQueryParam
      extends OptionalQueryParamDecoderMatcher[Int]("page")

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
