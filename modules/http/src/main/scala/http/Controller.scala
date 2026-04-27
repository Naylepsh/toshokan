package http

import cats.effect.IO
import cats.syntax.all.*
import org.http4s.*
import org.http4s.dsl.Http4sDsl

private type RouteHandler[A] = (A => IO[Response[IO]]) => IO[Response[IO]]

trait Routed:
  val routes: HttpRoutes[IO]

object Routed:
  def combine(controllers: List[Routed]): HttpRoutes[IO] =
    controllers
      .map(_.routes)
      .reduceOption(_ <+> _)
      .getOrElse(HttpRoutes.empty[IO])

abstract class Controller extends Http4sDsl[IO] with Routed:
  protected def withJsonErrorsHandled[A](
      request: Request[IO]
  )(using EntityDecoder[IO, A]): RouteHandler[A] = f =>
    request
      .as[A]
      .attempt
      .flatMap:
        case Left(InvalidMessageBodyFailure(details, cause)) =>
          BadRequest(cause.map(_.toString).getOrElse(details))
        case Left(error) =>
          scribe.error(error.toString)
          InternalServerError("Something went wrong")
        case Right(a) => f(a)

object Controller:
  given EntityEncoder[IO, scalatags.Text.TypedTag[String]] =
    EntityEncoder
      .stringEncoder[IO]
      .contramap[scalatags.Text.TypedTag[String]](_.render)
