package app.wiring

import scala.concurrent.duration.*

import assetScraping.configs.*
import assetScraping.downloading.*
import assetScraping.downloading.domain.{AssetEntryDir, DownloadDir}
import assetScraping.schedules.*
import assetScraping.{
  AssetScrapingController,
  AssetScrapingService,
  AssetScrapingView
}
import cats.effect.IO
import com.microsoft.playwright.Browser
import http.View.NavBarItem
import scraper.Scraper
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend

case class AssetScrapingModule[F[_]](
    scrapingService: AssetScrapingService[F],
    scrapingController: AssetScrapingController[F],
    configService: AssetScrapingConfigService[F],
    configController: AssetScrapingConfigController[F],
    downloadingService: AssetDownloadingService[F, AssetEntryDir],
    downloadingController: AssetDownloadingController[F],
    scheduleService: ScheduleService[F],
    scheduleController: ScheduleController[F]
)

object AssetScrapingModule:
  def make(
      library: LibraryModule[IO],
      externals: ExternalServices[IO],
      httpBackend: SttpBackend[IO, WebSockets],
      browser: Browser,
      downloadDir: DownloadDir,
      navBarItems: List[NavBarItem],
      xa: doobie.Transactor[IO]
  ): AssetScrapingModule[IO] =
    val configRepository = AssetScrapingConfigRepository.make[IO](xa)
    val configService = AssetScrapingConfigService
      .make[IO](configRepository, library.assetService)
    val configView = AssetScrapingConfigView(navBarItems)
    val configController =
      AssetScrapingConfigController(configService, configView)

    val scheduleRepository = ScheduleRepository.make[IO](xa)
    val scheduleService = ScheduleService.make(
      scheduleRepository,
      library.assetService,
      library.categoryService
    )
    val scheduleView = ScheduleView(navBarItems)
    val scheduleController = ScheduleController[IO](
      scheduleService,
      library.categoryService,
      scheduleView
    )

    val scraper     = Scraper.make[IO]
    val siteScraper = SiteScrapers.makeScraperPicker(httpBackend, browser)
    val scrapingService = AssetScrapingService.make[IO](
      library.assetService,
      library.assetRepository,
      configService,
      scheduleService,
      scraper,
      siteScraper
    )
    val scrapingView = AssetScrapingView(navBarItems)
    val scrapingController = AssetScrapingController[IO](
      scrapingService,
      library.categoryService,
      scrapingView
    )

    val localEntryStorage = EntryLocalStorage[IO](downloadDir)
    val downloadingService = AssetDownloadingService.make[IO, AssetEntryDir](
      externals.mangadexApi,
      httpBackend,
      library.assetRepository,
      localEntryStorage,
      AssetDownloadingService.Config(2.seconds)
    )
    val downloadingView = AssetDownloadingView(navBarItems)
    val downloadingController = AssetDownloadingController[IO](
      downloadingService,
      downloadingView
    )

    AssetScrapingModule(
      scrapingService = scrapingService,
      scrapingController = scrapingController,
      configService = configService,
      configController = configController,
      downloadingService = downloadingService,
      downloadingController = downloadingController,
      scheduleService = scheduleService,
      scheduleController = scheduleController
    )
