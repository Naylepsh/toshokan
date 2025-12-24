package app.middleware

import cats.effect.kernel.Sync
import org.http4s.HttpRoutes
import org.http4s.server.middleware.ErrorAction

def logErrors[F[_]: Sync](service: HttpRoutes[F]) =
  ErrorAction.httpRoutes[F](
    service,
    (_, error) => scribe.cats[F].error(error.toString)
  )
