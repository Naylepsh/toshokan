package app

import cats.MonadThrow
import cats.effect.kernel.Deferred
import cats.syntax.all.*
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Location
import org.http4s.server.Router

class ShutdownController[F[_]: MonadThrow](shutdownSignal: Deferred[F, Unit])
    extends Http4sDsl[F]:
  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F]:
    case GET -> Root =>
      shutdownSignal.complete(()) *> Response[F]()
        .withStatus(Status.Found)
        .withHeaders(Location(Uri.unsafeFromString("/assets")))
        .pure

  val routes = Router("shutdown" -> httpRoutes)
