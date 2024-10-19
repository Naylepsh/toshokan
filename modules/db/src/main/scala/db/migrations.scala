package db
package migrations

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.util.transactor.Transactor
import cats.effect.kernel.Sync

def migrationSQL[F[_]: Sync] = Sync[F].delay:
  new String(
    Files.readAllBytes(Paths.get("./db/schema.sql")),
    StandardCharsets.UTF_8
  )

def applyMigrations[F[_]: Sync](xa: Transactor[F]): F[Unit] =
  migrationSQL.flatMap: sql =>
    sql
      .split(";")
      .toList
      .filterNot(_.isBlank)
      .traverse: statement =>
        Fragment.const(statement).update.run
      .transact(xa)
      .void
