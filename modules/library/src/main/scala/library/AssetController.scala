package library

import cats.Monad
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.headers.*

class AssetController[F[_]: Monad, A](
    service: AssetService[F],
    view: AssetView[F, A]
)(using EntityEncoder[F, A]) extends Http4sDsl[F]:
  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      Ok(view.render(List.empty), `Content-Type`(view.mediaType))

  val routes = Router("assets" -> httpRoutes)
