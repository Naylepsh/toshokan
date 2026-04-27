package myAnimeList

import cats.effect.IO
import cats.mtl.Handle
import org.http4s.*
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.server.Router

class MyAnimeListController(service: MyAnimeListService) extends http.Controller:

  private val httpRoutes = HttpRoutes.of[IO]:
    case POST -> Root =>
      service.prepareForTokenAcquisition.flatMap(uri => Ok(uri.toString))

    case GET -> Root :? CodeQueryParamMatcher(code) =>
      Handle
        .allow[NoCodeChallenge]:
          service.acquireToken(code) *> Ok("")
        .rescue: error =>
          scribe.cats[IO].error(
            "acquireToken shouldn't be called without calling prepareForTokenAcquisition first"
          ) *> IO.raiseError(error)

  val routes = Router("myanimelist" -> httpRoutes)

private object CodeQueryParamMatcher
    extends QueryParamDecoderMatcher[String]("code")
