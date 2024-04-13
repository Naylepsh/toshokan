import assetScraping.AssetScrapingView
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*
import http.View.NavBarItem
import middleware.logErrors
import org.http4s.syntax.all.*
import scraper.Scraper
import sttp.client3.httpclient.cats.HttpClientCatsBackend

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    val serverConfig = ServerConfig.default
    val dbConfig     = db.Config.forSqlite(db.Path(sys.env("DATABASE_URL")))

    val navBarItems = List(
      NavBarItem("Assets", "/assets"),
      NavBarItem("New releases", "/assets/entries-by-release-date"),
      NavBarItem("Scraping", "/asset-scraping")
    )

    db.transactors.makeSqliteTransactorResource[IO](dbConfig).use: xa =>
      val assetRepository = library.AssetRepository.make[IO](xa)
      val assetService    = library.AssetService.make(assetRepository)
      val assetView       = library.AssetView(navBarItems)
      val assetController = library.AssetController(assetService, assetView)

      val scraper = Scraper.make[IO]

      val httpClient      = HttpClientCatsBackend.resource[IO]()
      val pickSiteScraper = SiteScrapers.makeScraperPicker(httpClient)

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

      HttpServer[IO]
        .newEmber(serverConfig, logErrors(routes).orNotFound)
        .useForever
