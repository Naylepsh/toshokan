package app

import assetScraping.configs.AssetScrapingConfigController
import assetScraping.schedules.*
import assetScraping.{AssetScrapingController, AssetScrapingView}
import cats.effect.*
import cats.effect.kernel.Resource
import cats.effect.std.Random
import com.microsoft.playwright.Browser
import doobie.Transactor
import http.Routed
import http.View.NavBarItem
import org.http4s.HttpRoutes
import org.http4s.syntax.all.*
import progressTracking.mal.{MalAuth, MyAnimeListClient}
import progressTracking.{
  ProgressTrackingController,
  ProgressTrackingService,
  ProgressTrackingView
}
import scraper.Scraper
import scraper.util.playwright
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend
import sttp.client3.httpclient.cats.HttpClientCatsBackend

import middleware.logErrors

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    for
      _ <- logging.init[IO]
      (serverConfig, dbConfig, snapshotConfig, malAuth, navBarItems) <- config
        .load[IO]
      random <- Random.scalaUtilRandom[IO]
      result <- setupResources(dbConfig).use:
        (xa, httpBackend, browser, shutdownSignal) =>
          val snapshotManager = snapshotConfig
            .map(snapshot.git.GitSnapshotManager[IO](_))
            .getOrElse(snapshot.NoopSnapshotManager[IO])
          createControllers(
            xa,
            httpBackend,
            browser,
            malAuth,
            shutdownSignal,
            navBarItems,
            random
          ).map(Routed.combine)
            .flatMap: routes =>
              snapshotManager.saveIfDue()
                *> startServer(serverConfig, routes, shutdownSignal)
    yield result

  private def setupResources(dbConfig: db.Config) =
    for
      xa          <- db.transactors.makeSqliteTransactorResource[IO](dbConfig)
      httpBackend <- HttpClientCatsBackend.resource[IO]()
      browser <- playwright
        .makePlaywrightResource[IO]
        .evalMap(p => IO.delay(p.chromium().launch()))
      shutdownSignal <- Resource.eval(Deferred[IO, Unit])
    yield (xa, httpBackend, browser, shutdownSignal)

  private def createControllers(
      xa: Transactor[IO],
      httpBackend: SttpBackend[IO, WebSockets],
      browser: Browser,
      malAuth: MalAuth,
      shutdownSignal: Deferred[IO, Unit],
      navBarItems: List[NavBarItem],
      random: Random[IO]
  ) =
    val categoryRepo = library.category.CategoryRepository.make[IO](xa)
    val categoryService =
      library.category.CategoryService.make[IO](categoryRepo)
    val assetRepo    = library.AssetRepository.make[IO](xa)
    val assetService = library.AssetService.make(assetRepo)
    val assetView    = library.AssetView(navBarItems)
    val assetController =
      library.AssetController(assetService, categoryService, assetView)

    val scraper         = Scraper.make[IO]
    val pickSiteScraper = SiteScrapers.makeScraperPicker(httpBackend, browser)

    val scheduleRepo = ScheduleRepository.make[IO](xa)
    val scheduleService =
      ScheduleService.make(scheduleRepo, assetService, categoryService)
    val scheduleView = ScheduleView(navBarItems)
    val scheduleController =
      ScheduleController[IO](scheduleService, categoryService, scheduleView)

    val assetScrapingConfigRepo =
      assetScraping.configs.AssetScrapingConfigRepository.make[IO](xa)
    val assetScrapingConfigService = assetScraping.configs.AssetScrapingService
      .make[IO](assetScrapingConfigRepo, assetService)
    val assetScrapingConfigView =
      assetScraping.configs.AssetScrapingConfigView(navBarItems)
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
    val assetScrapingView = AssetScrapingView(navBarItems)
    val assetScrapingController = assetScraping.AssetScrapingController[IO](
      assetScrapingService,
      categoryService,
      assetScrapingView
    )

    val malClient = MyAnimeListClient.make[IO](httpBackend, malAuth, random)
    ProgressTrackingService
      .make(xa, malClient, assetService, categoryService)
      .map: progressTrackingService =>
        val progressTrackingView = ProgressTrackingView(navBarItems)
        val progressTrackingController = ProgressTrackingController(
          assetService,
          progressTrackingService,
          progressTrackingView
        )

        val publicController   = PublicController[IO]()
        val shutdownController = ShutdownController[IO](shutdownSignal)

        List(
          assetController,
          assetScrapingController,
          assetScrapingConfigController,
          scheduleController,
          progressTrackingController,
          publicController,
          shutdownController
        )

  private def startServer(
      serverConfig: ServerConfig,
      routes: HttpRoutes[IO],
      shutdownSignal: Deferred[IO, Unit]
  ) =
    HttpServer[IO]
      .newEmber(serverConfig, logErrors(routes).orNotFound)
      .use(_ => IO.never.race(shutdownSignal.get).void)
