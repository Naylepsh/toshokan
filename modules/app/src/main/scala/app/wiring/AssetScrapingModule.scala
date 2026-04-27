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

case class AssetScrapingModule(
    scrapingService: AssetScrapingService,
    scrapingController: AssetScrapingController,
    configService: AssetScrapingConfigService,
    configController: AssetScrapingConfigController,
    authorConfigService: AuthorScrapingConfigService,
    authorConfigController: AuthorScrapingConfigController,
    downloadingService: AssetDownloadingService[AssetEntryDir],
    downloadingController: AssetDownloadingController,
    scheduleService: ScheduleService,
    scheduleController: ScheduleController
)

object AssetScrapingModule:
  def make(
      library: LibraryModule,
      externals: ExternalServices,
      httpBackend: SttpBackend[IO, WebSockets],
      browser: Browser,
      downloadDir: DownloadDir,
      navBarItems: List[NavBarItem],
      xa: doobie.Transactor[IO]
  ): AssetScrapingModule =
    val configRepository = AssetScrapingConfigRepository.make
    val configService =
      AssetScrapingConfigService.make(
        configRepository,
        library.assetService,
        xa
      )
    val configView = AssetScrapingConfigView(navBarItems)
    val configController =
      AssetScrapingConfigController(configService, configView)

    val authorConfigRepository = AuthorScrapingConfigRepository.make
    val authorConfigService =
      AuthorScrapingConfigService.make(
        authorConfigRepository,
        library.authorRepository,
        xa
      )
    val authorConfigView = AuthorScrapingConfigView(navBarItems)
    val authorConfigController =
      AuthorScrapingConfigController(
        authorConfigService,
        library.authorRepository,
        authorConfigView,
        xa
      )

    val scheduleRepository = ScheduleRepository.make
    val scheduleService =
      ScheduleService.make(
        scheduleRepository,
        library.assetService,
        library.categoryService,
        xa
      )
    val scheduleView = ScheduleView(navBarItems)
    val scheduleController =
      ScheduleController(scheduleService, library.categoryService, scheduleView)

    val scraper      = Scraper.make
    val siteScrapers = SiteScrapers.make(httpBackend, browser)
    val scrapingService = AssetScrapingService.make(
      library.assetService,
      library.assetRepository,
      configService,
      authorConfigService,
      library.authorRepository,
      scheduleService,
      scraper,
      siteScrapers.forAsset,
      siteScrapers.forAuthor,
      xa
    )
    val scrapingView = AssetScrapingView(navBarItems)
    val scrapingController = AssetScrapingController(
      scrapingService,
      library.categoryService,
      scrapingView
    )

    val localEntryStorage = EntryLocalStorage(downloadDir)
    val downloadingService = AssetDownloadingService.make[AssetEntryDir](
      externals.mangadexApi,
      httpBackend,
      library.assetRepository,
      localEntryStorage,
      AssetDownloadingService.Config(2.seconds),
      xa
    )
    val downloadingView = AssetDownloadingView(navBarItems)
    val downloadingController =
      AssetDownloadingController(downloadingService, downloadingView)

    AssetScrapingModule(
      scrapingService = scrapingService,
      scrapingController = scrapingController,
      configService = configService,
      configController = configController,
      authorConfigService = authorConfigService,
      authorConfigController = authorConfigController,
      downloadingService = downloadingService,
      downloadingController = downloadingController,
      scheduleService = scheduleService,
      scheduleController = scheduleController
    )
