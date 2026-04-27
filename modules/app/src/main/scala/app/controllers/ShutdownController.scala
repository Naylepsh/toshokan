package app.controllers

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.syntax.all.*
import org.http4s.*
import org.http4s.headers.Location
import org.http4s.server.Router

class ShutdownController(shutdownSignal: Deferred[IO, Unit])
    extends http.Controller:
  private val httpRoutes: HttpRoutes[IO] = HttpRoutes.of[IO]:
    case GET -> Root =>
      shutdownSignal.complete(()) *> Response[IO]()
        .withStatus(Status.Found)
        .withHeaders(Location(Uri.unsafeFromString("/assets")))
        .pure

  val routes = Router("shutdown" -> httpRoutes)
