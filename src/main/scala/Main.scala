import cats.effect.{ IO, IOApp }
import cats.syntax.all.*
import org.http4s.syntax.all.*

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    val serverConfig = ServerConfig.default
    val dbConfig = db.Config.forSqlite(
      db.Path("sqlite:///home/naylepsh/dev/toshokan/db.sqlite")
    )

    db.transactors.makeSqliteTransactorResource[IO](dbConfig).use: xa =>
      val assetRepository = library.AssetRepository.make[IO](xa)
      val assetService    = library.AssetService.make(assetRepository)
      val assetView       = library.AssetView.makeHtmlView[IO]
      val assetController = library.AssetController(assetService, assetView)

      val assetScrapingRepository =
        assetScraping.AssetScrapingRepository.make[IO](xa)
      val assetScrapingService = assetScraping.AssetScrapingService.make[IO](
        assetScrapingRepository,
        assetService
      )
      val assetScrapingView = assetScraping.AssetScrapingView.makeHtmlView[IO]
      val assetScrapingController = assetScraping.AssetScrapingController(
        assetScrapingService,
        assetScrapingView
      )

      val publicController = PublicController[IO]()

      val routes =
        assetController.routes <+> assetScrapingController.routes <+> publicController.routes

      HttpServer[IO]
        .newEmber(serverConfig, routes.orNotFound)
        .useForever
