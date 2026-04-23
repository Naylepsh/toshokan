package app.wiring

import assetMapping.*
import cats.effect.IO
import doobie.Transactor
import http.View.NavBarItem

case class AssetMappingModule[F[_]](
    service: AssetMappingService[F],
    controller: AssetMappingController[F]
)

object AssetMappingModule:
  def make(
      library: LibraryModule[IO],
      malModule: MyAnimeListModule[IO],
      xa: Transactor[IO],
      navBarItems: List[NavBarItem]
  ): AssetMappingModule[IO] =
    val service = AssetMappingService(
      library.assetService,
      library.categoryService,
      malModule.service,
      MalMangaMappingRepository.make,
      xa
    )
    val view       = AssetMappingView(navBarItems)
    val controller = AssetMappingController(service, library.assetService, view)

    AssetMappingModule(
      service = service,
      controller = controller
    )
