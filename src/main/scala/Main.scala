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

      val publicController = PublicController[IO]()

      val routes = assetController.routes <+> publicController.routes

      HttpServer[IO]
        .newEmber(serverConfig, routes.orNotFound)
        .useForever
