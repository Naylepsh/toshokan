package app

import cats.MonadThrow
import cats.syntax.all.*
import fs2.io.file.Files
import http.Routed
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

class PublicController[F[_]: MonadThrow: Files]
    extends Http4sDsl[F]
    with Routed[F]:
  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F]:
    case request @ GET -> path =>
      val p = fs2.io.file.Path(s"./public/$path")
      StaticFile.fromPath(p, request.some).getOrElseF(NotFound())

  val routes = Router("public" -> httpRoutes)

