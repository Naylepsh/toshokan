package db
package migrations

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.util.transactor.Transactor

def migrationSQL: IO[String] = IO.delay:
  new String(
    Files.readAllBytes(Paths.get("./db/schema.sql")),
    StandardCharsets.UTF_8
  )

def applyMigrations(xa: Transactor[IO]): IO[Unit] =
  migrationSQL.flatMap: sql =>
    sql
      .split(";")
      .toList
      .filterNot(_.isBlank)
      .traverse: statement =>
        Fragment.const(statement).update.run
      .transact(xa)
      .void
