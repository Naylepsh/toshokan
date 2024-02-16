package assetScraping

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import core.Tuples
import doobie.*
import doobie.implicits.*
import doobiex.*
import library.domain.AssetId

import domain.*

trait AssetScrapingRepository[F[_]]:
  def add(scrapingConfig: NewAssetScrapingConfig)
      : F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]]
  def delete(scrapingConfigId: AssetScrapingConfigId): F[Unit]

object AssetScrapingRepository:
  object AssetScrapingConfigs extends TableDefinition("asset_scraping_configs"):
    val id        = Column[AssetScrapingConfigId]("id")
    val uri       = Column[ScrapingConfigUri]("uri")
    val isEnabled = Column[IsConfigEnabled]("is_enabled")
    val site      = Column[Site]("site")
    val assetId   = Column[AssetId]("asset_id")

    val *           = Columns((id, uri, site, isEnabled, assetId))
    val allExceptId = Columns((uri, site, isEnabled, assetId))

  def make[F[_]: MonadCancelThrow](xa: Transactor[F])
      : AssetScrapingRepository[F] = new:
    def add(scrapingConfig: NewAssetScrapingConfig)
        : F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]] =
      exists(scrapingConfig.uri).flatMap: configExists =>
        if configExists then
          AddScrapingConfigError.ConfigAlreadyExists.asLeft.pure
        else addWithoutChecking(scrapingConfig).map(_.asRight)

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

    private def addWithoutChecking(scrapingConfig: NewAssetScrapingConfig)
        : F[ExistingAssetScrapingConfig] =
      val values = Tuples.to(scrapingConfig)
      sql"""
        INSERT INTO ${AssetScrapingConfigs}(${AssetScrapingConfigs.allExceptId}) 
        VALUES ($values) 
        RETURNING ${AssetScrapingConfigs.*}"""
        .queryOf(AssetScrapingConfigs.*)
        .unique
        .transact(xa)
        .map(Tuples.from[ExistingAssetScrapingConfig](_))
