import assetScraping.AssetScrapingView
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import middleware.logErrors
import org.http4s.syntax.all.*
import scraper.Scraper
import sttp.client3.httpclient.cats.HttpClientCatsBackend

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    load[IO].flatMap: (serverConfig, dbConfig, snapshotConfig, navBarItems) =>
      (
        db.transactors.makeSqliteTransactorResource[IO](dbConfig),
        HttpClientCatsBackend.resource[IO]()
      ).tupled.use: (xa, httpBackend) =>
        val assetRepository = library.AssetRepository.make[IO](xa)
        val assetService    = library.AssetService.make(assetRepository)
        val assetView       = library.AssetView(navBarItems)
        val assetController = library.AssetController(assetService, assetView)

        val scraper = Scraper.make[IO]

        val pickSiteScraper = SiteScrapers.makeScraperPicker(httpBackend)

        val assetScrapingRepository =
          assetScraping.AssetScrapingRepository.make[IO](xa)
        val assetScrapingService = assetScraping.AssetScrapingService.make[IO](
          assetScrapingRepository,
          assetService,
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

        val routes =
          assetController.routes <+> assetScrapingController.routes <+> publicController.routes

        val snapshotManager =
          snapshotConfig
            .map(snapshot.git.GitSnapshotManager[IO](_))
            .getOrElse(snapshot.NoopSnapshotManager[IO])

        HttpServer[IO]
          .newEmber(serverConfig, logErrors(routes).orNotFound)
          .evalTap: _ =>
            snapshotManager.saveIfDue()
          .useForever
