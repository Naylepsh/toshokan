package library

import cats.effect.IOApp
import cats.effect.IO
import cats.syntax.all.*
import cats.effect.syntax.all.*
import io.github.arainko.ducktape.*
import core.Tuples
import library.domain.*

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    val config = database.Config(
      "sqlite:///home/naylepsh/dev/toshokan/db.sqlite",
      "",
      ""
    )
    database.makeSqliteTransactorResource[IO](config).use: xa =>
      val repository = AssetRepository.make(xa)
      repository.add(NewAsset(AssetTitle("Hello"))).flatMap(IO.println)
