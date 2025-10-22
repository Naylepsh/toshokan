package myAnimeList

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Sync
import cats.mtl.Handle
import cats.syntax.all.*
import org.http4s.*
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.server.Router

class MyAnimeListController[F[_]: MonadCancelThrow: Sync](
    service: MyAnimeListService[F]
) extends http.Controller[F]:

  private val httpRoutes = HttpRoutes.of[F]:
    case POST -> Root =>
      service.prepareForTokenAcquisition.flatMap(uri => Ok(uri.toString))

    case GET -> Root :? CodeQueryParamMatcher(code) =>
      Handle
        .allow[NoCodeChallenge]:
          service.acquireToken(code) *> Ok("")
        .rescue: error =>
          scribe
            .cats[F]
            .error(
              "acquireToken shouldn't be called without calling prepareForTokenAcquisition first"
            ) *> MonadCancelThrow[F].raiseError(error)

  val routes = Router("myanimelist" -> httpRoutes)

private object CodeQueryParamMatcher
    extends QueryParamDecoderMatcher[String]("code")
