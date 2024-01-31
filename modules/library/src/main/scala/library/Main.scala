package library

import cats.effect.IOApp
import cats.effect.IO
import cats.syntax.all.*
import cats.effect.syntax.all.*

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    val config = database.Config(
      "sqlite:///home/naylepsh/dev/toshokan/db.sqlite",
      "",
      ""
    )
    database.makeSqliteTransactorResource[IO](config).use: xa =>
      for
        column  <- AssetRepository.testSelectColumn(xa)
        columns <- AssetRepository.testSelectAllColumns(xa)
        _ = println(column)
        _ = println(columns)
      yield ()
