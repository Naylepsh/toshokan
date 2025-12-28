package app.wiring

import cats.effect.IO
import doobie.Transactor
import http.View.NavBarItem
import progressTracking.*

case class ProgressTrackingModule[F[_]](
    service: ProgressTrackingService[F],
    controller: ProgressTrackingController[F]
)

object ProgressTrackingModule:
  def make(
      library: LibraryModule[IO],
      malModule: MyAnimeListModule[IO],
      mappingModule: AssetMappingModule[IO],
      navBarItems: List[NavBarItem],
      xa: Transactor[IO]
  ): ProgressTrackingModule[IO] =
    val entryProgressRepository = EntryProgressRepository.make(xa)
    val service = ProgressTrackingService.make(
      malModule.service,
      library.assetService,
      mappingModule.service,
      entryProgressRepository
    )
    val view       = ProgressTrackingView(navBarItems)
    val controller = ProgressTrackingController(service, view)

    ProgressTrackingModule(
      service = service,
      controller = controller
    )
