package app.wiring

import assetImporting.{
  AssetImportingController,
  AssetImportingService,
  AssetImportingView
}
import cats.effect.IO
import doobie.Transactor
import http.View.NavBarItem

case class AssetImportingModule[F[_]](
    service: AssetImportingService[F],
    controller: AssetImportingController[F]
)

object AssetImportingModule:
  def make(
      library: LibraryModule[IO],
      scraping: AssetScrapingModule[IO],
      mapping: AssetMappingModule[IO],
      externals: ExternalServices[IO],
      navBarItems: List[NavBarItem],
      xa: Transactor[IO]
  ): AssetImportingModule[IO] =
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
