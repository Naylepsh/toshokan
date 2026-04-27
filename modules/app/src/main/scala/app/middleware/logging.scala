package app.middleware

import cats.effect.IO
import org.http4s.HttpRoutes
import org.http4s.server.middleware.ErrorAction

def logErrors(service: HttpRoutes[IO]) =
  ErrorAction.httpRoutes[IO](
    service,
    (_, error) => scribe.cats[IO].error(error.toString)
  )
