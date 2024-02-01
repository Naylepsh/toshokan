package library

import cats.effect.IOApp
import cats.effect.IO
import cats.syntax.all.*
import cats.effect.syntax.all.*
import io.github.arainko.ducktape.*
import core.Tuples
import library.domain.*
import java.net.URI

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    val config = database.Config(
      "sqlite:///home/naylepsh/dev/toshokan/db.sqlite",
      "",
      ""
    )
    database.makeSqliteTransactorResource[IO](config).use: xa =>
      val repository = AssetRepository.make(xa)
      val newAsset   = NewAsset(AssetTitle("Hello"))
      def newEntry(assetId: Long) = NewAssetEntry(
        EntryNo("42"),
        EntryUri(URI("http://localhost:8080/hello")),
          assetId
      )
      for
        asset <- repository.add(newAsset)
        _     <- IO.println(asset)
        entry <- asset.fold(
          _ => IO.unit,
          asset => repository.addEntry(newEntry(asset.id))
        )
        _ <- IO.println(entry)
      yield ()
