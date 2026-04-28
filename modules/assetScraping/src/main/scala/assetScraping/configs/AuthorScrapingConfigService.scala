package assetScraping.configs

import cats.effect.IO
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import core.syntax.*
import doobie.*
import doobie.implicits.*
import library.author.AuthorRepository
import library.author.domain.*

import domain.*

trait AuthorScrapingConfigService:
  def findAllEnabled: IO[List[ExistingAuthorScrapingConfig]]
  def findByAuthorId(authorId: AuthorId): IO[List[ExistingAuthorScrapingConfig]]
  def add(
      scrapingConfig: NewAuthorScrapingConfig
  ): Raise[IO, AddAuthorScrapingConfig] ?=> IO[ExistingAuthorScrapingConfig]
  def delete(id: AuthorScrapingConfigId): IO[Unit]
  def updateEnabled(
      id: AuthorScrapingConfigId,
      enabled: IsConfigEnabled
  ): IO[Boolean]

object AuthorScrapingConfigService:
  def make(
      repository: AuthorScrapingConfigRepository,
      authorRepository: AuthorRepository,
      xa: Transactor[IO]
  ): AuthorScrapingConfigService = new:

    override def findAllEnabled: IO[List[ExistingAuthorScrapingConfig]] =
      repository.findAllEnabled.transact(xa)

    override def findByAuthorId(
        authorId: AuthorId
    ): IO[List[ExistingAuthorScrapingConfig]] =
      repository.findByAuthorId(authorId).transact(xa)

    override def add(
        scrapingConfig: NewAuthorScrapingConfig
    ): Raise[IO, AddAuthorScrapingConfig] ?=> IO[ExistingAuthorScrapingConfig] =
      for
        _ <- authorRepository
          .find(scrapingConfig.authorId)
          .transact(xa)
          .someOrRaise(AddAuthorScrapingConfig.AuthorDoesNotExist)
        result <- repository
          .add(scrapingConfig)
          .transact(xa)
          .flatMap:
            case Right(config) => config.pure
            case Left(error)   => error.raise
      yield result

    override def delete(id: AuthorScrapingConfigId): IO[Unit] =
      repository.delete(id).transact(xa).void

    override def updateEnabled(
        id: AuthorScrapingConfigId,
        enabled: IsConfigEnabled
    ): IO[Boolean] =
      repository.updateEnabled(id, enabled).transact(xa)
