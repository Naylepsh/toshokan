package app

import assetScraping.AssetScrapingView
import assetScraping.schedules.*
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import org.http4s.syntax.all.*
import scraper.Scraper
import scraper.util.playwright
import sttp.client3.httpclient.cats.HttpClientCatsBackend

import middleware.logErrors

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(minimumLevel = Some(scribe.Level.Debug))
      .replace()

    load[IO].flatMap: (serverConfig, dbConfig, snapshotConfig, navBarItems) =>
      (
        db.transactors.makeSqliteTransactorResource[IO](dbConfig),
        HttpClientCatsBackend.resource[IO](),
        playwright
          .makePlaywrightResource[IO]
          .evalMap(p => IO.delay(p.chromium().launch()))
      ).tupled.use: (xa, httpBackend, browser) =>

        val categoryRepository =
          library.category.CategoryRepository.make[IO](xa)
        val categoryService =
          library.category.CategoryService.make[IO](categoryRepository)
        val assetRepository = library.AssetRepository.make[IO](xa)
        val assetService    = library.AssetService.make(assetRepository)
        val assetView       = library.AssetView(navBarItems)
        val assetController =
          library.AssetController(assetService, categoryService, assetView)

        val scraper = Scraper.make[IO]
        val pickSiteScraper =
          SiteScrapers.makeScraperPicker(httpBackend, browser)

        val scheduleRepository = ScheduleRepository.make[IO](xa)
        val scheduleService = ScheduleService.make(
          scheduleRepository,
          assetService,
          categoryService
        )
        val scheduleView = ScheduleView(navBarItems)
        val scheduleController =
          ScheduleController[IO](
            scheduleService,
            categoryService,
            scheduleView
          )

        val assetScrapingConfigRepository =
          assetScraping.configs.AssetScrapingConfigRepository.make[IO](xa)
        val assetScrapingConfigService =
          assetScraping.configs.AssetScrapingService
            .make[IO](assetScrapingConfigRepository, assetService)
        val assetScrapingConfigView = assetScraping.configs
          .AssetScrapingConfigView(navBarItems = navBarItems)
        val assetScrapingConfigController =
          assetScraping.configs.AssetScrapingConfigController(
            assetScrapingConfigService,
            assetScrapingConfigView
          )

        val assetScrapingService = assetScraping.AssetScrapingService.make[IO](
          assetService,
          assetScrapingConfigService,
          scheduleService,
          scraper,
          pickSiteScraper
        )
        val assetScrapingView = AssetScrapingView(navBarItems = navBarItems)
        val assetScrapingController =
          assetScraping.AssetScrapingController[IO](
            assetScrapingService,
            assetScrapingView
          )

        val publicController = PublicController[IO]()

        val routes = assetController.routes
          <+> assetScrapingController.routes
          <+> assetScrapingConfigController.routes
          <+> scheduleController.routes
          <+> publicController.routes

        val snapshotManager =
          snapshotConfig
            .map(snapshot.git.GitSnapshotManager[IO](_))
            .getOrElse(snapshot.NoopSnapshotManager[IO])

        HttpServer[IO]
          .newEmber(serverConfig, logErrors(routes).orNotFound)
          .evalTap: _ =>
            snapshotManager.saveIfDue()
          .useForever
