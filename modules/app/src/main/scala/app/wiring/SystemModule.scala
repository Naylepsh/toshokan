package app.wiring

import app.controllers.{PublicController, ShutdownController}
import cats.effect.IO
import cats.effect.kernel.Deferred

case class SystemModule(
    publicController: PublicController,
    shutdownController: ShutdownController
)

object SystemModule:
  def make(shutdownSignal: Deferred[IO, Unit]): SystemModule =
    val publicController   = PublicController()
    val shutdownController = ShutdownController(shutdownSignal)

    SystemModule(
      publicController = publicController,
      shutdownController = shutdownController
    )
