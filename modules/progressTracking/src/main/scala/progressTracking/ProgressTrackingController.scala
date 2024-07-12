package progressTracking

import cats.effect.{Sync, MonadCancelThrow}
import cats.syntax.all.*
import library.AssetController.AssetIdVar
import library.AssetService
import org.http4s.*
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.headers.*
import org.http4s.server.Router

import domain.Term

class ProgressTrackingController[F[_]: MonadCancelThrow: Sync](
    assetService: AssetService[F],
    service: ProgressTrackingService[F],
    view: ProgressTrackingView
) extends http.Controller[F]:
  import http.Controller.given

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root / AssetIdVar(id) :? TermQueryParam(term) =>
      service
        .searchForManga(term)
        .flatMap:
          case Left(error) => MonadCancelThrow[F].raiseError(error)
          case Right(mangaMatches) =>
            Ok(
              view.mangaMatchesPartial(mangaMatches),
              `Content-Type`(MediaType.text.html)
            )

    case GET -> Root / AssetIdVar(id) =>
      assetService
        .find(id)
        .flatMap:
          case None => NotFound(s"No manga with id=$id found")
          case Some(asset, _) =>
            Ok(
              view.renderMangaSearch(asset.title, asset.id),
              `Content-Type`(MediaType.text.html)
            )

    case POST -> Root / "mal" =>
      service.prepareForTokenAcqusition.flatMap(uri => Ok(uri.toString))

    case GET -> Root / "mal" :? CodeQueryParamMatcher(code) =>
      service
        .acquireToken(code)
        .flatMap(_.fold(MonadCancelThrow[F].raiseError, _ => Ok("")))

  val routes = Router("progress-tracking" -> httpRoutes)

given QueryParamDecoder[Term] = QueryParamDecoder[String].map(Term(_))

object TermQueryParam extends QueryParamDecoderMatcher[Term]("term")

object CodeQueryParamMatcher extends QueryParamDecoderMatcher[String]("code")
