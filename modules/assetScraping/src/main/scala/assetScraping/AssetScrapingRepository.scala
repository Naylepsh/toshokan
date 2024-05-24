package assetScraping

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import core.Tuples
import doobie.*
import doobie.implicits.*
import doobiex.*
import library.domain.AssetId

import domain.configs.*
import scrapes.domain.*

trait AssetScrapingRepository[F[_]]:
  def findAllEnabled: F[List[ExistingAssetScrapingConfig]]
  def findByAssetId(assetId: AssetId): F[List[ExistingAssetScrapingConfig]]
  def add(
      scrapingConfig: NewAssetScrapingConfig
  ): F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]]
  def update(
      scrapingConfig: ExistingAssetScrapingConfig
  ): F[Either[UpdateScrapingConfigError, ExistingAssetScrapingConfig]]
  def delete(scrapingConfigId: AssetScrapingConfigId): F[Unit]
  def addOrUpdate(
      assetIds: NonEmptyList[AssetId],
      scrapeDate: PastScrapeCreatedAt
  ): F[Unit]

object AssetScrapingRepository:
  private object AssetScrapingConfigs
      extends TableDefinition("asset_scraping_configs"):
    val id        = Column[AssetScrapingConfigId]("id")
    val uri       = Column[ScrapingConfigUri]("uri")
    val isEnabled = Column[IsConfigEnabled]("is_enabled")
    val site      = Column[Site]("site")
    val assetId   = Column[AssetId]("asset_id")

    val *           = Columns((id, uri, site, isEnabled, assetId))
    val allExceptId = Columns((uri, site, isEnabled, assetId))

  private object PastScrapes extends TableDefinition("past_scrapes"):
    val id        = Column[Long]("id")
    val assetId   = Column[AssetId]("asset_id")
    val createdAt = Column[PastScrapeCreatedAt]("created_at")

    val * = Columns((assetId, createdAt))

  def make[F[_]: MonadCancelThrow](
      xa: Transactor[F]
  ): AssetScrapingRepository[F] = new:
    def findAllEnabled: F[List[ExistingAssetScrapingConfig]] =
      sql"""
        SELECT ${AssetScrapingConfigs.*}
        FROM ${AssetScrapingConfigs}
        WHERE ${AssetScrapingConfigs.isEnabled === IsConfigEnabled(true)}"""
        .queryOf(AssetScrapingConfigs.*)
        .to[List]
        .transact(xa)
        .map(_.map(Tuples.from[ExistingAssetScrapingConfig](_)))

    def findByAssetId(assetId: AssetId): F[List[ExistingAssetScrapingConfig]] =
      sql"""
        SELECT ${AssetScrapingConfigs.*}
        FROM ${AssetScrapingConfigs}
        WHERE ${AssetScrapingConfigs.assetId === assetId}"""
        .queryOf(AssetScrapingConfigs.*)
        .to[List]
        .transact(xa)
        .map(_.map(Tuples.from[ExistingAssetScrapingConfig](_)))

    def add(
        scrapingConfig: NewAssetScrapingConfig
    ): F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]] =
      exists(scrapingConfig.uri).flatMap: configExists =>
        if configExists then
          AddScrapingConfigError.ConfigAlreadyExists.asLeft.pure
        else addWithoutChecking(scrapingConfig).map(_.asRight)

    def update(
        scrapingConfig: ExistingAssetScrapingConfig
    ): F[Either[UpdateScrapingConfigError, ExistingAssetScrapingConfig]] =
      exists(scrapingConfig.uri).flatMap: configExists =>
        if configExists then
          updateWithoutChecking(scrapingConfig).map(_.asRight)
        else UpdateScrapingConfigError.ConfigDoesNotExist.asLeft.pure

    def delete(scrapingConfigId: AssetScrapingConfigId): F[Unit] =
      sql"""
        DELETE FROM ${AssetScrapingConfigs} 
        WHERE ${AssetScrapingConfigs.id === scrapingConfigId}
      """.update.run.transact(xa).void

    def addOrUpdate(
        assetIds: NonEmptyList[AssetId],
        scrapeDate: PastScrapeCreatedAt
    ): F[Unit] =
      val values = assetIds.map: assetId =>
        fr"(${assetId}, ${scrapeDate})"
      val valuesFrag = values.tail.foldLeft(values.head)(_ ++ fr"," ++ _)

      sql"""
      INSERT INTO ${PastScrapes}(${PastScrapes.assetId}, ${PastScrapes.createdAt})
      VALUES ${valuesFrag}
      ON CONFLICT ${PastScrapes.assetId} DO UPDATE SET
        ${PastScrapes.createdAt === scrapeDate}
      """.update.run.transact(xa).void

    private def exists(uri: ScrapingConfigUri): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${AssetScrapingConfigs}
        WHERE ${AssetScrapingConfigs.uri === uri}
      """.query[Int].option.transact(xa).map(_.isDefined)

    private def addWithoutChecking(
        scrapingConfig: NewAssetScrapingConfig
    ): F[ExistingAssetScrapingConfig] =
      val values = Tuples.to(scrapingConfig)
      sql"""
        INSERT INTO ${AssetScrapingConfigs}(${AssetScrapingConfigs.allExceptId}) 
        VALUES ($values) 
        RETURNING ${AssetScrapingConfigs.*}"""
        .queryOf(AssetScrapingConfigs.*)
        .unique
        .transact(xa)
        .map(Tuples.from[ExistingAssetScrapingConfig](_))

    private def updateWithoutChecking(
        scrapingConfig: ExistingAssetScrapingConfig
    ): F[ExistingAssetScrapingConfig] =
      sql"""
      UPDATE ${AssetScrapingConfigs}
      SET ${AssetScrapingConfigs.isEnabled === scrapingConfig.isEnabled},
        ${AssetScrapingConfigs.uri === scrapingConfig.uri},
        ${AssetScrapingConfigs.site === scrapingConfig.site}
      WHERE ${AssetScrapingConfigs.id === scrapingConfig.id}
      """.update.run.transact(xa).as(scrapingConfig)
