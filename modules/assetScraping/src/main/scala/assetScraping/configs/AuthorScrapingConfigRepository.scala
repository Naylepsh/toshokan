package assetScraping.configs

import assetScraping.configs.domain.*
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import core.Tuples
import core.given
import db.fragments.insertIntoReturning
import doobie.*
import doobie.implicits.*
import doobie.util.transactor.Transactor
import library.author.domain.AuthorId
import neotype.interop.doobie.given

trait AuthorScrapingConfigRepository[F[_]]:
  def add(
      scrapingConfig: NewAuthorScrapingConfig
  ): Raise[F, AddAuthorScrapingConfig] ?=> F[ExistingAuthorScrapingConfig]
  def findAllEnabled: F[List[ExistingAuthorScrapingConfig]]

  def findByAuthorId(authorId: AuthorId): F[List[ExistingAuthorScrapingConfig]]
  def findById(
      id: AuthorScrapingConfigId
  ): F[Option[ExistingAuthorScrapingConfig]]
  def delete(id: AuthorScrapingConfigId): F[Boolean]
  def updateEnabled(
      id: AuthorScrapingConfigId,
      enabled: IsConfigEnabled
  ): F[Boolean]

object AuthorScrapingConfigRepository:
  def make[F[_]: MonadCancelThrow](
      xa: Transactor[F]
  ): AuthorScrapingConfigRepository[F] = new:
    def add(
        scrapingConfig: NewAuthorScrapingConfig
    ): (Raise[F, AddAuthorScrapingConfig]) ?=> F[ExistingAuthorScrapingConfig] =
      exists(scrapingConfig.uri).flatMap:
        case true  => AddAuthorScrapingConfig.ConfigAlreadyExists.raise
        case false => addWithoutChecking(scrapingConfig)

    def findAllEnabled: F[List[ExistingAuthorScrapingConfig]] =
      sql"""
      SELECT ${AuthorScrapingConfigs.*}
      FROM ${AuthorScrapingConfigs}
      WHERE ${AuthorScrapingConfigs.isEnabled === IsConfigEnabled(true)}
      """
        .queryOf(AuthorScrapingConfigs.*)
        .to[List]
        .transact(xa)
        .map(_.map(Tuples.from[ExistingAuthorScrapingConfig](_)))

    def findByAuthorId(
        authorId: AuthorId
    ): F[List[ExistingAuthorScrapingConfig]] =
      sql"""
      SELECT ${AuthorScrapingConfigs.*}
      FROM ${AuthorScrapingConfigs}
      WHERE ${AuthorScrapingConfigs.authorId == authorId}
      """
        .queryOf(AuthorScrapingConfigs.*)
        .to[List]
        .transact(xa)
        .map(_.map(Tuples.from[ExistingAuthorScrapingConfig](_)))

    def findById(
        id: AuthorScrapingConfigId
    ): F[Option[ExistingAuthorScrapingConfig]] =
      sql"""
      SELECT ${AuthorScrapingConfigs.*}
      FROM ${AuthorScrapingConfigs}
      WHERE ${AuthorScrapingConfigs.id == id}
      """
        .queryOf(AuthorScrapingConfigs.*)
        .option
        .transact(xa)
        .map(_.map(Tuples.from[ExistingAuthorScrapingConfig](_)))

    def delete(id: AuthorScrapingConfigId): F[Boolean] =
      sql"""
      DELETE FROM ${AuthorScrapingConfigs}
      WHERE ${AuthorScrapingConfigs.id == id}
      """.update.run.transact(xa).map(_ > 0)

    def updateEnabled(
        id: AuthorScrapingConfigId,
        enabled: IsConfigEnabled
    ): F[Boolean] =
      sql"""
      UPDATE ${AuthorScrapingConfigs}
      SET ${AuthorScrapingConfigs.isEnabled} = $enabled
      WHERE ${AuthorScrapingConfigs.id == id}
      """.update.run.transact(xa).map(_ > 0)

    private def exists(uri: ScrapingConfigUri): F[Boolean] =
      sql"""
      SELECT 1
      FROM ${AuthorScrapingConfigs}
      WHERE ${AuthorScrapingConfigs.uri == uri}
      """.query[Int].option.transact(xa).map(_.isDefined)

    private def addWithoutChecking(scrapingConfig: NewAuthorScrapingConfig) =
      insertIntoReturning(
        AuthorScrapingConfigs,
        NonEmptyList.of(
          _.uri --> scrapingConfig.uri,
          _.isEnabled --> scrapingConfig.isEnabled,
          _.site --> scrapingConfig.site,
          _.authorId --> scrapingConfig.authorId
        ),
        _.*
      )
        .queryOf(AuthorScrapingConfigs.*)
        .unique
        .transact(xa)
        .map(Tuples.from[ExistingAuthorScrapingConfig](_))

private object AuthorScrapingConfigs
    extends TableDefinition("author_scraping_configs"):
  val id        = Column[AuthorScrapingConfigId]("id")
  val uri       = Column[ScrapingConfigUri]("uri")
  val isEnabled = Column[IsConfigEnabled]("is_enabled")
  val site      = Column[AuthorSite]("site")
  val authorId  = Column[AuthorId]("author_id")

  val *           = Columns((id, uri, site, isEnabled, authorId))
  val allExceptId = Columns((uri, site, isEnabled, authorId))
