package app.wiring

import assetImporting.{
  AssetImportingController,
  AssetImportingService,
  AssetImportingView
}
import cats.effect.IO
import doobie.Transactor
import http.View.NavBarItem

case class AssetImportingModule(
    service: AssetImportingService,
    controller: AssetImportingController
)

object AssetImportingModule:
  def make(
      library: LibraryModule,
      scraping: AssetScrapingModule,
      mapping: AssetMappingModule,
      externals: ExternalServices,
      navBarItems: List[NavBarItem],
      xa: Transactor[IO]
  ): AssetImportingModule =
    val service = AssetImportingService(
      library.assetService,
      library.categoryService,
      mapping.service,
      scraping.configService,
      library.authorRepository,
      externals.mangadexApi,
      xa
    )
    val view       = AssetImportingView(navBarItems)
    val controller = AssetImportingController(service, view)

    AssetImportingModule(
      service = service,
      controller = controller
    )
