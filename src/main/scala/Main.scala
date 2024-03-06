import cats.effect.{ IO, IOApp }
import cats.syntax.all.*
import org.http4s.syntax.all.*
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import scraper.Scraper

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    val serverConfig = ServerConfig.default
    val dbConfig = db.Config.forSqlite(
      db.Path("sqlite:///home/naylepsh/dev/toshokan/db.sqlite")
    )

    db.transactors.makeSqliteTransactorResource[IO](dbConfig).use: xa =>
      val assetRepository = library.AssetRepository.make[IO](xa)
      val assetService    = library.AssetService.make(assetRepository)
      val assetController = library.AssetController(assetService)

      val scraper = Scraper.make[IO]

      val httpClient = HttpClientCatsBackend.resource[IO]()
      val pickSiteScraper = SiteScrapers.makeScraperPicker(httpClient)


      val assetScrapingRepository =
        assetScraping.AssetScrapingRepository.make[IO](xa)
      val assetScrapingService = assetScraping.AssetScrapingService.make[IO](
        assetScrapingRepository,
        assetService,
        scraper,
        pickSiteScraper
      )
      val assetScrapingController =
        assetScraping.AssetScrapingController[IO](assetScrapingService)

      val publicController = PublicController[IO]()

      val routes =
        assetController.routes <+> assetScrapingController.routes <+> publicController.routes

      HttpServer[IO]
        .newEmber(serverConfig, routes.orNotFound)
        .useForever
