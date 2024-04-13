package middleware

import cats.MonadThrow
import cats.effect.std.Console
import org.http4s.server.middleware.ErrorAction
import org.http4s.{ HttpRoutes, * }

def logErrors[F[_]: MonadThrow: Console](service: HttpRoutes[F]) =
  ErrorAction.httpRoutes[F](service, (req, error) => Console[F].println(error.getMessage))
