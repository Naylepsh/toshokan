package progressTracking

import cats.effect.{Concurrent, MonadCancelThrow}
import cats.syntax.all.*
import library.AssetController.{AssetIdVar, EntryIdVar}
import library.domain.{AssetId, UpdateEntryError}
import myAnimeList.domain.Term
import org.http4s.*
import org.http4s.dsl.impl.{
  OptionalQueryParamDecoderMatcher,
  QueryParamDecoderMatcher
}
import org.http4s.headers.*
import org.http4s.server.Router

import schemas.{*, given}
import viewComponents.Pagination

class ProgressTrackingController[F[_]: MonadCancelThrow: Concurrent](
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

    case PATCH -> Root / "partials" / "releases" / AssetIdVar(assetId) =>
      service.binge(assetId) *> Ok("Binged")

  val routes = Router("progress-tracking" -> httpRoutes)

given QueryParamDecoder[Term] = QueryParamDecoder[String].map(Term.apply)

private object TermQueryParam extends QueryParamDecoderMatcher[Term]("term")

private object CodeQueryParamMatcher
    extends QueryParamDecoderMatcher[String]("code")

private object OptionalPageQueryParam
    extends OptionalQueryParamDecoderMatcher[Int]("page")
