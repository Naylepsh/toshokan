package assetScraping.configs

import cats.data.NonEmptyList
import cats.syntax.all.*
import core.Tuples
import core.given
import db.fragments.*
import doobie.*
import doobie.implicits.*
import library.asset.domain.AssetId
import neotype.interop.doobie.given

import domain.*

trait AssetScrapingConfigRepository:
  def findAllEnabled: ConnectionIO[List[ExistingAssetScrapingConfig]]
  def findByAssetId(
      assetId: AssetId
  ): ConnectionIO[List[ExistingAssetScrapingConfig]]
  def add(
      scrapingConfig: NewAssetScrapingConfig
  ): ConnectionIO[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]]
  def update(
      scrapingConfig: ExistingAssetScrapingConfig
  ): ConnectionIO[
    Either[UpdateScrapingConfigError, ExistingAssetScrapingConfig]
  ]
  def delete(scrapingConfigId: AssetScrapingConfigId): ConnectionIO[Unit]
  def transferConfigs(
      sourceAssetId: AssetId,
      targetAssetId: AssetId
  ): ConnectionIO[Unit]

object AssetScrapingConfigRepository:

  val make: AssetScrapingConfigRepository = new:
    def findAllEnabled: ConnectionIO[List[ExistingAssetScrapingConfig]] =
      sql"""
        SELECT ${AssetScrapingConfigs.*}
        FROM ${AssetScrapingConfigs}
        WHERE ${AssetScrapingConfigs.isEnabled === IsConfigEnabled(true)}"""
        .queryOf(AssetScrapingConfigs.*)
        .to[List]
        .map(_.map(Tuples.from[ExistingAssetScrapingConfig](_)))

    def findByAssetId(
        assetId: AssetId
    ): ConnectionIO[List[ExistingAssetScrapingConfig]] =
      sql"""
        SELECT ${AssetScrapingConfigs.*}
        FROM ${AssetScrapingConfigs}
        WHERE ${AssetScrapingConfigs.assetId === assetId}"""
        .queryOf(AssetScrapingConfigs.*)
        .to[List]
        .map(_.map(Tuples.from[ExistingAssetScrapingConfig](_)))

    def add(
        scrapingConfig: NewAssetScrapingConfig
    ): ConnectionIO[
      Either[AddScrapingConfigError, ExistingAssetScrapingConfig]
    ] =
      exists(scrapingConfig.uri).flatMap:
        case true =>
          AddScrapingConfigError.ConfigAlreadyExists.asLeft.pure[ConnectionIO]
        case false => addWithoutChecking(scrapingConfig).map(_.asRight)

    def update(
        scrapingConfig: ExistingAssetScrapingConfig
    ): ConnectionIO[
      Either[UpdateScrapingConfigError, ExistingAssetScrapingConfig]
    ] =
      val id = scrapingConfig.id
      (exists(scrapingConfig.id), idOf(scrapingConfig.uri)).tupled.flatMap:
        case (true, Some(`id`)) =>
          updateWithoutChecking(scrapingConfig).map(_.asRight)
        case (true, None) =>
          updateWithoutChecking(scrapingConfig).map(_.asRight)
        case (false, _) =>
          UpdateScrapingConfigError.ConfigDoesNotExist.asLeft.pure[ConnectionIO]
        case (_, Some(_)) =>
          UpdateScrapingConfigError.ConflictingConfigError.asLeft
            .pure[ConnectionIO]

    def delete(scrapingConfigId: AssetScrapingConfigId): ConnectionIO[Unit] =
      sql"""
        DELETE FROM ${AssetScrapingConfigs} 
        WHERE ${AssetScrapingConfigs.id === scrapingConfigId}
      """.update.run.void

    def transferConfigs(
        sourceAssetId: AssetId,
        targetAssetId: AssetId
    ): ConnectionIO[Unit] =
      for _ <- sql"""
          INSERT OR IGNORE INTO ${AssetScrapingConfigs}
            (${AssetScrapingConfigs.uri}, ${AssetScrapingConfigs.site}, ${AssetScrapingConfigs.isEnabled}, ${AssetScrapingConfigs.assetId})
          SELECT ${AssetScrapingConfigs.uri}, ${AssetScrapingConfigs.site}, ${AssetScrapingConfigs.isEnabled}, $targetAssetId
          FROM ${AssetScrapingConfigs}
          WHERE ${AssetScrapingConfigs.assetId} = $sourceAssetId
            AND ${AssetScrapingConfigs.uri} NOT IN (
              SELECT ${AssetScrapingConfigs.uri} FROM ${AssetScrapingConfigs}
              WHERE ${AssetScrapingConfigs.assetId} = $targetAssetId
            )
        """.update.run
      yield ()

    private def idOf(
        uri: ScrapingConfigUri
    ): ConnectionIO[Option[AssetScrapingConfigId]] =
      sql"""
        SELECT ${AssetScrapingConfigs.id}
        FROM ${AssetScrapingConfigs}
        WHERE ${AssetScrapingConfigs.uri === uri}
      """.query[AssetScrapingConfigId].option

    private def exists(uri: ScrapingConfigUri): ConnectionIO[Boolean] =
      sql"""
        SELECT 1
        FROM ${AssetScrapingConfigs}
        WHERE ${AssetScrapingConfigs.uri === uri}
      """.query[Int].option.map(_.isDefined)

    private def exists(id: AssetScrapingConfigId): ConnectionIO[Boolean] =
      sql"""
        SELECT 1
        FROM ${AssetScrapingConfigs}
        WHERE ${AssetScrapingConfigs.id === id}
      """.query[Int].option.map(_.isDefined)

    private def addWithoutChecking(
        scrapingConfig: NewAssetScrapingConfig
    ): ConnectionIO[ExistingAssetScrapingConfig] =
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
        .map(Tuples.from[ExistingAssetScrapingConfig](_))

    private def updateWithoutChecking(
        scrapingConfig: ExistingAssetScrapingConfig
    ): ConnectionIO[ExistingAssetScrapingConfig] =
      sql"""
      ${AssetScrapingConfigs.updateTable(
          NonEmptyList.of(
            AssetScrapingConfigs.isEnabled --> scrapingConfig.isEnabled,
            AssetScrapingConfigs.uri --> scrapingConfig.uri,
            AssetScrapingConfigs.site --> scrapingConfig.site
          )
        )}
      WHERE ${AssetScrapingConfigs.id === scrapingConfig.id}
      """.update.run.as(scrapingConfig)

private object AssetScrapingConfigs
    extends TableDefinition("asset_scraping_configs"):
  val id        = Column[AssetScrapingConfigId]("id")
  val uri       = Column[ScrapingConfigUri]("uri")
  val isEnabled = Column[IsConfigEnabled]("is_enabled")
  val site      = Column[Site]("site")
  val assetId   = Column[AssetId]("asset_id")

  val *           = Columns((id, uri, site, isEnabled, assetId))
  val allExceptId = Columns((uri, site, isEnabled, assetId))
