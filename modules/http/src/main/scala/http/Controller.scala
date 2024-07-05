package http

import cats.MonadThrow
import cats.syntax.all.*
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import cats.effect.kernel.Sync

private type RouteHandler[F[_], A] = (A => F[Response[F]]) => F[Response[F]]

trait Routed[F[_]]:
  val routes: HttpRoutes[F]

object Routed:
  def combine[F[_]: Sync](
      controllers: List[Routed[F]]
  ): HttpRoutes[F] =
    controllers
      .map(_.routes)
      .reduceOption(_ <+> _)
      .getOrElse(HttpRoutes.empty[F])

abstract class Controller[F[_]: MonadThrow] extends Http4sDsl[F] with Routed[F]:
  protected def withJsonErrorsHandled[A](
      request: Request[F]
  )(using EntityDecoder[F, A]): RouteHandler[F, A] = f =>
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
  given [F[_]]: EntityEncoder[F, scalatags.Text.TypedTag[String]] =
    EntityEncoder
      .stringEncoder[F]
      .contramap[scalatags.Text.TypedTag[String]](_.render)
