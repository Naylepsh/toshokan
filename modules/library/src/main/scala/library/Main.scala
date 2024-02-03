package library

import java.net.URI

import cats.effect.syntax.all.*
import cats.effect.{ IO, IOApp }
import cats.syntax.all.*
import core.Tuples
import db.*
import io.github.arainko.ducktape.*
import library.domain.*
import java.time.LocalDate

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    val config = Config(
      Path("sqlite:///home/naylepsh/dev/toshokan/db.sqlite"),
      Username(""),
      Password("")
    )
    transactors.makeSqliteTransactorResource[IO](config).use: xa =>
      val repository = AssetRepository.make(xa)
      val newAsset   = NewAsset(AssetTitle("Hello"))
      def newEntry(assetId: AssetId) = NewAssetEntry(
        EntryNo("42"),
        EntryUri(URI("http://localhost:8080/hello")),
        WasEntrySeen(false),
        DateUploaded(LocalDate.now()),
        assetId
      )
      for
        asset <- repository.add(newAsset)
        _     <- IO.println(asset)
        entry <- asset.fold(
          _ => IO.unit,
          asset => repository.addEntry(newEntry(asset.id))
        )
        _   <- IO.println(entry)
        all <- repository.findAll
        _   <- IO.print(all)
      yield ()
