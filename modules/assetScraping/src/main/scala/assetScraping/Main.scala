package assetScraping

import java.nio.file.Path

import assetScraping.downloading.domain.DownloadDir
import cats.effect.{IO, IOApp}
import library.domain.EntryId
import mangadex.MangadexApi
import sttp.client3.httpclient.cats.HttpClientCatsBackend

import downloading.AssetDownloadingService

object Main extends IOApp.Simple:
  override def run: IO[Unit] =
    val databaseUrl = sys.env("DATABASE_URL")
    val dbConfig    = db.Config.forSqlite(db.Path(databaseUrl))
    val resources = for
      xa          <- db.transactors.makeSqliteTransactorResource[IO](dbConfig)
      httpBackend <- HttpClientCatsBackend.resource[IO]()
    yield (xa, httpBackend)

    resources.use: (xa, backend) =>
      val assetRepo   = library.AssetRepository.make[IO](xa)
      val mangadexApi = MangadexApi.make(backend)
      val downloadDir =
        DownloadDir(Path.of("/home/naylepsh/dev/toshokan/data/"))
      val service = AssetDownloadingService
        .make[IO](mangadexApi, backend, downloadDir, assetRepo)
      service.download(EntryId(8236)).void
