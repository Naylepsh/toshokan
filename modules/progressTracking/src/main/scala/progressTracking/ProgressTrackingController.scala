package progressTracking

import cats.effect.{Concurrent, MonadCancelThrow}
import cats.syntax.all.*
import library.AssetController.{AssetIdVar, EntryIdVar}
import library.AssetService
import library.domain.{AssetId, UpdateEntryError}
import org.http4s.*
import org.http4s.dsl.impl.{
  OptionalQueryParamDecoderMatcher,
  QueryParamDecoderMatcher
}
import org.http4s.headers.*
import org.http4s.server.Router

import domain.Term
import schemas.{*, given}
import viewComponents.Pagination

class ProgressTrackingController[F[_]: MonadCancelThrow: Concurrent](
    assetService: AssetService[F],
    service: ProgressTrackingService[F],
    view: ProgressTrackingView
) extends http.Controller[F]:
  import http.Controller.given

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root / "releases" :? OptionalPageQueryParam(page) =>
      service.findNotSeenReleases.attempt.flatMap:
        case Left(reason) =>
          InternalServerError("Something went wrong")
        case Right(releases) =>
          val (items, pagination) =
            Pagination.paginate(releases, page.getOrElse(1))
          Ok(
            view.renderReleases(items, pagination).toString,
            `Content-Type`(MediaType.text.html)
          )

    case GET -> Root / "partials" / "releases" :? OptionalPageQueryParam(
          page
        ) =>
      service.findNotSeenReleases.attempt.flatMap:
        case Left(reason) =>
          InternalServerError("Something went wrong")
        case Right(releases) =>
          val (items, pagination) =
            Pagination.paginate(releases, page.getOrElse(1))
          Ok(
            view.releasesPartial(items, pagination).toString,
            `Content-Type`(MediaType.text.html)
          )

    case req @ PUT -> Root
        / "partials"
        / "releases"
        / AssetIdVar(assetId)
        / EntryIdVar(entryId) =>
      withJsonErrorsHandled[UpdateProgressDTO](req): dto =>
        service
          .updateProgress(assetId, entryId, dto.wasEntrySeen)
          .flatMap:
            case Left(UpdateEntryError.AssetDoesNotExists) =>
              NotFound("Asset does not exist")
            case Left(UpdateEntryError.EntryDoesNotExist) =>
              NotFound("Entry does not exist")
            case Right(asset, entry) => Ok(view.entryPartial(asset, entry))

    case GET -> Root / "mal" / "manga-mapping" / AssetIdVar(id) / "search" =>
      assetService
        .find(id)
        .flatMap:
          case None => NotFound(s"No manga with id=$id found")
          case Some(asset, _) =>
            Ok(
              view.renderMangaSearch(asset.title, asset.id),
              `Content-Type`(MediaType.text.html)
            )

    case GET -> Root / "mal" / "manga-mapping" / AssetIdVar(id)
        / "search" :? TermQueryParam(term) =>
      service
        .searchForManga(term)
        .flatMap:
          case Left(error) => MonadCancelThrow[F].raiseError(error)
          case Right(mangaMatches) =>
            Ok(
              view.mangaMatchesPartial(id, mangaMatches),
              `Content-Type`(MediaType.text.html)
            )

    case GET -> Root / "mal" / "manga-mapping" / AssetIdVar(id) =>
      service
        .findAssetWithMalMapping(id)
        .flatMap:
          case Left(error) =>
            MonadCancelThrow[F].raiseError(error)
          case Right(None) =>
            SeeOther(
              Location(
                Uri.unsafeFromString(
                  s"/progress-tracking/mal/manga-mapping/${id}/search"
                )
              )
            )
          case Right(Some(asset, mapping)) =>
            Ok(
              view.renderMangaMalMapping(asset, mapping),
              `Content-Type`(MediaType.text.html)
            )

    case req @ POST -> Root / "mal" / "manga-mapping" =>
      withJsonErrorsHandled[NewMalMangaMappingDTO](req): newMalLinking =>
        service
          .assignExternalIdToManga(newMalLinking.malId, newMalLinking.assetId)
          .flatMap:
            case Left(AssetNotFound) => NotFound("Asset not found")
            case Left(CategoryNotFound | AssetIsNotManga) =>
              BadRequest("Asset illegible for MAL integration")
            case Left(
                  ExternalIdAlreadyInUse | MangaAlreadyHasExternalIdAssigned
                ) =>
              Conflict("Duplicate mangaId / externalId assignment")
            case Right(_) => Ok("")

    case DELETE -> Root / "mal" / "manga-mapping" / AssetIdVar(id) =>
      service.deleteMapping(id) *> Ok("")

    case POST -> Root / "mal" =>
      service.prepareForTokenAcquisition.flatMap(uri => Ok(uri.toString))

    case GET -> Root / "mal" :? CodeQueryParamMatcher(code) =>
      service
        .acquireToken(code)
        .flatMap(_.fold(MonadCancelThrow[F].raiseError, _ => Ok("")))

  val routes = Router("progress-tracking" -> httpRoutes)

given QueryParamDecoder[Term] = QueryParamDecoder[String].map(Term(_))

private object TermQueryParam extends QueryParamDecoderMatcher[Term]("term")

private object CodeQueryParamMatcher
    extends QueryParamDecoderMatcher[String]("code")

private object OptionalPageQueryParam
    extends OptionalQueryParamDecoderMatcher[Int]("page")
