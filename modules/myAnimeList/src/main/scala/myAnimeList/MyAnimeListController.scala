package myAnimeList

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import org.http4s.*
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.server.Router

class MyAnimeListController[F[_]: MonadCancelThrow](
    service: MyAnimeListService[F]
) extends http.Controller[F]:

  private val httpRoutes = HttpRoutes.of[F]:
    case POST -> Root =>
      service.prepareForTokenAcquisition.flatMap(uri => Ok(uri.toString))

    case GET -> Root :? CodeQueryParamMatcher(code) =>
      service
        .acquireToken(code)
        .flatMap(_.fold(MonadCancelThrow[F].raiseError, _ => Ok("")))

  val routes = Router("myanimelist" -> httpRoutes)

private object CodeQueryParamMatcher
    extends QueryParamDecoderMatcher[String]("code")
