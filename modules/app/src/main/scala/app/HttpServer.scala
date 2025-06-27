package app

import cats.Applicative
import cats.effect.*
import com.comcast.ip4s.*
import fs2.io.net.Network
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.http4s.server.defaults.Banner

case class ServerConfig(host: Host, port: Port)
object ServerConfig:
  val default: ServerConfig = ServerConfig(ipv4"0.0.0.0", port"8080")

trait HttpServer[F[_]]:
  def newEmber(config: ServerConfig, app: HttpApp[F]): Resource[F, Server]

object HttpServer:
  def apply[F[_]: HttpServer]: HttpServer[F] = summon

  given given_HttpServer[F[_]: Async: Network]: HttpServer[F] with
    def newEmber(config: ServerConfig, app: HttpApp[F]): Resource[F, Server] =
      EmberServerBuilder
        .default[F]
        .withHost(config.host)
        .withPort(config.port)
        .withHttpApp(app)
        .build
        .evalTap(showEmberBanner)

  private def showEmberBanner[F[_]: Applicative](server: Server) =
    Applicative[F].pure(
      println(
        s"\n${Banner.mkString("\n")}\nHTTP Server started at ${server.address}"
      )
    )
