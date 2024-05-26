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
        val assetController = library.AssetController(assetService, assetView)

        val scraper = Scraper.make[IO]

        val pickSiteScraper =
          SiteScrapers.makeScraperPicker(httpBackend, browser)
        val scheduleRepository = ScheduleRepository.make[IO](xa)
        val scheduleService = ScheduleService.make(
          scheduleRepository,
          assetService,
          categoryService
        )

        val schedulesRepository = ScheduleRepository.make[IO](xa)
        val schedulesService = ScheduleService.make(
          schedulesRepository,
          assetService,
          categoryService
        )
        val schedulesView = ScheduleView(navBarItems)
        val schedulesController =
          ScheduleController[IO](
            schedulesService,
            categoryService,
            schedulesView
          )

        val assetScrapingRepository =
          assetScraping.AssetScrapingRepository.make[IO](xa)
        val assetScrapingService = assetScraping.AssetScrapingService.make[IO](
          assetScrapingRepository,
          assetService,
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
          <+> schedulesController.routes
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
