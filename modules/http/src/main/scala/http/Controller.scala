package http

import cats.MonadThrow
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.*

private type RouteHandler[F[_], A] = (A => F[Response[F]]) => F[Response[F]]

abstract class Controller[F[_]: MonadThrow] extends Http4sDsl[F]:
  protected def withJsonErrorsHandled[A](request: Request[F])(using
  EntityDecoder[F, A]): RouteHandler[F, A] = f =>
    request.as[A].attempt.flatMap:
      case Left(InvalidMessageBodyFailure(details, cause)) =>
        BadRequest(cause.map(_.toString).getOrElse(details))
      case Left(error) =>
        println(s"[ERROR]: $error")
        InternalServerError("Something went wrong")
      case Right(a) => f(a)
