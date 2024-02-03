package library

import cats.{Monad, MonadError}
import cats.syntax.all.*
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.headers.*

type MET[F[_]] = MonadError[F, Throwable]

class AssetController[F[_]: Monad: MET, A](
    service: AssetService[F],
    view: AssetView[F, A]
)(using EntityEncoder[F, A]) extends Http4sDsl[F]:
  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      service.findAll.attempt.flatMap: 
        case Right(assetsWithEntries) =>
          Ok(view.render(assetsWithEntries), `Content-Type`(view.mediaType))
        case Left(error) => 
          println(error)
          Ok("???")

  val routes = Router("assets" -> httpRoutes)
