package app.wiring

import app.controllers.{PublicController, ShutdownController}
import cats.effect.IO
import cats.effect.kernel.Deferred

case class SystemModule[F[_]](
    publicController: PublicController[F],
    shutdownController: ShutdownController[F]
)

object SystemModule:
  def make(shutdownSignal: Deferred[IO, Unit]): SystemModule[IO] =
    val publicController   = PublicController[IO]()
    val shutdownController = ShutdownController[IO](shutdownSignal)

    SystemModule(
      publicController = publicController,
      shutdownController = shutdownController
    )
