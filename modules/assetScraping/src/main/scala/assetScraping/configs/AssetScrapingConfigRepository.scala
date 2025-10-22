package assetScraping.configs

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import core.Tuples
import doobie.*
import doobie.implicits.*
import doobiex.*
import library.domain.AssetId

import domain.*

trait AssetScrapingConfigRepository[F[_]]:
  def findAllEnabled: F[List[ExistingAssetScrapingConfig]]
  def findByAssetId(assetId: AssetId): F[List[ExistingAssetScrapingConfig]]
  def add(
      scrapingConfig: NewAssetScrapingConfig
  ): Raise[F, AddScrapingConfigError] ?=> F[ExistingAssetScrapingConfig]
  def update(
      scrapingConfig: ExistingAssetScrapingConfig
  ): Raise[F, UpdateScrapingConfigError] ?=> F[ExistingAssetScrapingConfig]
  def delete(scrapingConfigId: AssetScrapingConfigId): F[Unit]

object AssetScrapingConfigRepository:

  def make[F[_]: MonadCancelThrow](
      xa: Transactor[F]
  ): AssetScrapingConfigRepository[F] = new:
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
    ): Raise[F, AddScrapingConfigError] ?=> F[ExistingAssetScrapingConfig] =
      exists(scrapingConfig.uri).flatMap: configExists =>
        if configExists then AddScrapingConfigError.ConfigAlreadyExists.raise
        else addWithoutChecking(scrapingConfig)

    def update(
        scrapingConfig: ExistingAssetScrapingConfig
    ): Raise[F, UpdateScrapingConfigError] ?=> F[ExistingAssetScrapingConfig] =
      (exists(scrapingConfig.id), exists(scrapingConfig.uri)).tupled.flatMap:
        case (true, false) =>
          updateWithoutChecking(scrapingConfig)
        case (false, _) =>
          UpdateScrapingConfigError.ConfigDoesNotExist.raise
        case (_, true) =>
          UpdateScrapingConfigError.ConflictingConfigError.raise

    def delete(scrapingConfigId: AssetScrapingConfigId): F[Unit] =
      sql"""
        DELETE FROM ${AssetScrapingConfigs} 
        WHERE ${AssetScrapingConfigs.id === scrapingConfigId}
      """.update.run.transact(xa).void

    private def exists(uri: ScrapingConfigUri): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${AssetScrapingConfigs}
        WHERE ${AssetScrapingConfigs.uri === uri}
      """.query[Int].option.transact(xa).map(_.isDefined)

    private def exists(id: AssetScrapingConfigId): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${AssetScrapingConfigs}
        WHERE ${AssetScrapingConfigs.id === id}
      """.query[Int].option.transact(xa).map(_.isDefined)

    private def addWithoutChecking(
        scrapingConfig: NewAssetScrapingConfig
    ): F[ExistingAssetScrapingConfig] =
      insertIntoReturning(
        AssetScrapingConfigs,
        NonEmptyList.of(
          _.uri --> scrapingConfig.uri,
          _.isEnabled --> scrapingConfig.isEnabled,
          _.site --> scrapingConfig.site,
          _.assetId --> scrapingConfig.assetId
        ),
        _.*
      )
        .queryOf(AssetScrapingConfigs.*)
        .unique
        .transact(xa)
        .map(Tuples.from[ExistingAssetScrapingConfig](_))

    private def updateWithoutChecking(
        scrapingConfig: ExistingAssetScrapingConfig
    ): F[ExistingAssetScrapingConfig] =
      sql"""
      ${updateTable(
          AssetScrapingConfigs,
          NonEmptyList.of(
            _.isEnabled --> scrapingConfig.isEnabled,
            _.uri --> scrapingConfig.uri,
            _.site --> scrapingConfig.site
          )
        )}
      WHERE ${AssetScrapingConfigs.id === scrapingConfig.id}
      """.update.run.transact(xa).as(scrapingConfig)

private object AssetScrapingConfigs
    extends TableDefinition("asset_scraping_configs"):
  val id        = Column[AssetScrapingConfigId]("id")
  val uri       = Column[ScrapingConfigUri]("uri")
  val isEnabled = Column[IsConfigEnabled]("is_enabled")
  val site      = Column[Site]("site")
  val assetId   = Column[AssetId]("asset_id")

  val *           = Columns((id, uri, site, isEnabled, assetId))
  val allExceptId = Columns((uri, site, isEnabled, assetId))
