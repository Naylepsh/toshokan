package app

import cats.effect.*
import com.comcast.ip4s.*
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.http4s.server.defaults.Banner

case class ServerConfig(host: Host, port: Port)
object ServerConfig:
  val default: ServerConfig = ServerConfig(ipv4"0.0.0.0", port"8080")

object HttpServer:
  def newEmber(config: ServerConfig, app: HttpApp[IO]): Resource[IO, Server] =
    EmberServerBuilder
      .default[IO]
      .withHost(config.host)
      .withPort(config.port)
      .withHttpApp(app)
      .build
      .evalTap: server =>
        IO.println(
          s"\n${Banner.mkString("\n")}\nHTTP Server started at ${server.address}"
        )
