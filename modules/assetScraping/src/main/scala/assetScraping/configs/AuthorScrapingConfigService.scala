package assetScraping.configs

import cats.effect.kernel.Sync
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import library.author.AuthorRepository
import library.author.domain.*

import domain.*

trait AuthorScrapingConfigService[F[_]]:
  def findAllEnabled: F[List[ExistingAuthorScrapingConfig]]
  def findByAuthorId(authorId: AuthorId): F[List[ExistingAuthorScrapingConfig]]
  def add(
      scrapingConfig: NewAuthorScrapingConfig
  ): Raise[F, AddAuthorScrapingConfig] ?=> F[ExistingAuthorScrapingConfig]
  def delete(id: AuthorScrapingConfigId): F[Unit]
  def updateEnabled(
      id: AuthorScrapingConfigId,
      enabled: IsConfigEnabled
  ): F[Boolean]

object AuthorScrapingConfigService:
  def make[F[_]: Sync](
      repository: AuthorScrapingConfigRepository[F],
      authorRepository: AuthorRepository[F]
  ): AuthorScrapingConfigService[F] = new:

    override def findAllEnabled: F[List[ExistingAuthorScrapingConfig]] =
      repository.findAllEnabled

    override def findByAuthorId(
        authorId: AuthorId
    ): F[List[ExistingAuthorScrapingConfig]] =
      repository.findByAuthorId(authorId)

    override def add(
        scrapingConfig: NewAuthorScrapingConfig
    ): Raise[F, AddAuthorScrapingConfig] ?=> F[ExistingAuthorScrapingConfig] =
      authorRepository
        .find(scrapingConfig.authorId)
        .flatMap:
          case Some(_) => repository.add(scrapingConfig)
          case None    => AddAuthorScrapingConfig.AuthorDoesNotExist.raise

    override def delete(id: AuthorScrapingConfigId): F[Unit] =
      repository.delete(id).void

    override def updateEnabled(
        id: AuthorScrapingConfigId,
        enabled: IsConfigEnabled
    ): F[Boolean] =
      repository.updateEnabled(id, enabled)
