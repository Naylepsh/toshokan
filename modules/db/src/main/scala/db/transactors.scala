package db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*

object transactors:
  def makeSqliteTransactorResource[F[_]: Async](
      config: Config
  ): Resource[F, Transactor[F]] =
    for
      ce <- ExecutionContexts.fixedThreadPool[F](32)
      xa <- HikariTransactor.newHikariTransactor[F](
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
