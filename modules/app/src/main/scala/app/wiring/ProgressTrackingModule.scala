package app.wiring

import cats.effect.IO
import doobie.Transactor
import http.View.NavBarItem
import progressTracking.*

case class ProgressTrackingModule(
    service: ProgressTrackingService,
    controller: ProgressTrackingController
)

object ProgressTrackingModule:
  def make(
      library: LibraryModule,
      malModule: MyAnimeListModule,
      mappingModule: AssetMappingModule,
      navBarItems: List[NavBarItem],
      xa: Transactor[IO]
  ): ProgressTrackingModule =
    val entryProgressRepository = EntryProgressRepository.make
    val service = ProgressTrackingService.make(
      malModule.service,
      library.assetService,
      mappingModule.service,
      entryProgressRepository,
      library.authorRepository,
      xa
    )
    val view       = ProgressTrackingView(navBarItems)
    val controller = ProgressTrackingController(service, view)

    ProgressTrackingModule(
      service = service,
      controller = controller
    )
