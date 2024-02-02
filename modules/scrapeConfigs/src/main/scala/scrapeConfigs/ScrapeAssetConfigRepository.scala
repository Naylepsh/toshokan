package scrapeConfigs

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import core.Tuples
import doobie.*
import doobie.implicits.*
import doobiex.*

import domain.*

trait ScrapeAssetConfigRepository[F[_]]:
  def findAll(assetId: AssetId): F[List[ExistingScrapeAssetConfig]]
  def add(config: NewScrapeAssetConfig)
      : F[Either[AddConfigError, ExistingScrapeAssetConfig]]

object ScrapeAssetConfigRepository:
  object ScrapeAssetConfigs extends TableDefinition("scrape_asset_configs"):
    val id        = Column[ScrapeAssetConfigId]("id")
    val assetId   = Column[AssetId]("asset_id")
    val isEnabled = Column[IsScrapeAssetConfigEnabled]("is_enabled")
    val uri       = Column[ScrapeAssetConfigUri]("uri")
    val site      = Column[Site]("site")

    val allExceptId = Columns((assetId, isEnabled, uri, site))
    val *           = Columns((id, assetId, isEnabled, uri, site))

  def make[F[_]: MonadCancelThrow](xa: Transactor[F])
      : ScrapeAssetConfigRepository[F] = new:

    def findAll(assetId: AssetId): F[List[ExistingScrapeAssetConfig]] =
      sql"""
        SELECT ${ScrapeAssetConfigs.*}
        FROM ${ScrapeAssetConfigs}
        WHERE ${ScrapeAssetConfigs.assetId} = ${assetId}
      """.queryOf(ScrapeAssetConfigs.*)
        .to[List]
        .transact(xa)
        .map: rows =>
          rows.map: row =>
            Tuples.from[ExistingScrapeAssetConfig](row)

    def add(config: NewScrapeAssetConfig)
        : F[Either[AddConfigError, ExistingScrapeAssetConfig]] =
      exists(config.uri).flatMap:
        case true  => AddConfigError.ConfigAlreadyExists.asLeft.pure
        case false => addWithoutChecking(config).map(_.asRight)

    private def addWithoutChecking(config: NewScrapeAssetConfig)
        : F[ExistingScrapeAssetConfig] =
      val values = Tuples.to(config)
      sql"""
        INSERT INTO ${ScrapeAssetConfigs}(${ScrapeAssetConfigs.allExceptId}) 
        VALUES ($values) 
        RETURNING ${ScrapeAssetConfigs.*}
      """.queryOf(ScrapeAssetConfigs.*)
        .unique
        .transact(xa)
        .map: row =>
          Tuples.from[ExistingScrapeAssetConfig](row)

    private def exists(uri: ScrapeAssetConfigUri): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${ScrapeAssetConfigs}
        WHERE ${ScrapeAssetConfigs.uri} = ${uri}
      """.query[Int].option.transact(xa).map(_.isDefined)
