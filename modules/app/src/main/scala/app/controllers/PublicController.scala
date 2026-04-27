package app.controllers

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.Files
import org.http4s.*
import org.http4s.server.Router

class PublicController extends http.Controller:
  private val httpRoutes: HttpRoutes[IO] = HttpRoutes.of[IO]:
    case request @ GET -> path =>
      val p = fs2.io.file.Path(s"./public/$path")
      StaticFile.fromPath(p, request.some).getOrElseF(NotFound())

  val routes = Router("public" -> httpRoutes)
