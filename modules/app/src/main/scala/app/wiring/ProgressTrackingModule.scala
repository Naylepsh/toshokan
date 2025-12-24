package app.wiring

import cats.effect.IO
import http.View.NavBarItem
import progressTracking.{
  ProgressTrackingController,
  ProgressTrackingService,
  ProgressTrackingView
}

case class ProgressTrackingModule[F[_]](
    service: ProgressTrackingService[F],
    controller: ProgressTrackingController[F]
)

object ProgressTrackingModule:
  def make(
      library: LibraryModule[IO],
      malModule: MyAnimeListModule[IO],
      mappingModule: AssetMappingModule[IO],
      navBarItems: List[NavBarItem]
  ): ProgressTrackingModule[IO] =
    val service = ProgressTrackingService.make(
      malModule.service,
      library.assetService,
      mappingModule.service
    )
    val view       = ProgressTrackingView(navBarItems)
    val controller = ProgressTrackingController(service, view)

    ProgressTrackingModule(
      service = service,
      controller = controller
    )
