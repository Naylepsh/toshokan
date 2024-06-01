package app.middleware

import cats.effect.kernel.Sync
import org.http4s.server.middleware.ErrorAction
import org.http4s.{HttpRoutes, *}

def logErrors[F[_]: Sync](service: HttpRoutes[F]) =
  ErrorAction.httpRoutes[F](
    service,
    (req, error) => scribe.cats[F].error(error.toString)
  )
