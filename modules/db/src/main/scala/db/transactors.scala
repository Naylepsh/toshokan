package db
package transactors

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.util.log.{LogEvent, LogHandler}

def makeSqliteTransactorResource(config: Config): Resource[IO, Transactor[IO]] =
  for
    ce <- ExecutionContexts.fixedThreadPool[IO](32)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.sqlite.JDBC",
      s"jdbc:${config.path}",
      config.username,
      config.password,
      ce
    )
    sqliteXa = Transactor.before.modify(
      xa,
      sql"PRAGMA foreign_keys=ON".update.run *> _
    )
  yield sqliteXa

def printSqlLogHandler: LogHandler[IO] = new:
  override def run(logEvent: LogEvent): IO[Unit] =
    IO.delay(println(logEvent.sql))

def inMemoryTransactor: Resource[IO, Transactor[IO]] =
  makeSqliteTransactorResource(Config.forSqlite(Path("sqlite::memory:")))
