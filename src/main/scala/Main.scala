import cats.effect.IOApp
import cats.effect.IO

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

      val routes = assetController.routes

      HttpServer[IO]
        .newEmber(serverConfig, routes.orNotFound)
        .useForever
