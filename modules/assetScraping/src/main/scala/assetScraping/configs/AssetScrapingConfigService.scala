package assetScraping.configs

import cats.effect.IO
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import library.asset.AssetService
import library.asset.domain.*

import domain.*

trait AssetScrapingConfigService:
  def findAllEnabled: IO[List[ExistingAssetScrapingConfig]]
  def findByAssetId(
      assetId: AssetId
  ): Raise[IO, FindScrapingConfigError] ?=> IO[
    (ExistingAsset, List[ExistingAssetScrapingConfig])
  ]
  def add(
      scrapingConfig: NewAssetScrapingConfig
  ): Raise[IO, AddScrapingConfigError] ?=> IO[ExistingAssetScrapingConfig]
  def update(
      scrapingConfig: ExistingAssetScrapingConfig
  ): Raise[IO, UpdateScrapingConfigError] ?=> IO[ExistingAssetScrapingConfig]
  def delete(id: AssetScrapingConfigId): IO[Unit]

object AssetScrapingConfigService:
  def make(
      repository: AssetScrapingConfigRepository,
      assetService: AssetService,
      xa: Transactor[IO]
  ): AssetScrapingConfigService = new:

    override def findAllEnabled: IO[List[ExistingAssetScrapingConfig]] =
      repository.findAllEnabled.transact(xa)

    override def findByAssetId(
        assetId: AssetId
    ): Raise[IO, FindScrapingConfigError] ?=> IO[
      (ExistingAsset, List[ExistingAssetScrapingConfig])
    ] =
      assetService
        .find(assetId)
        .flatMap:
          case Some(asset, _) =>
            repository.findByAssetId(assetId).transact(xa).map(asset -> _)
          case None => FindScrapingConfigError.AssetDoesNotExists.raise

    override def add(
        scrapingConfig: NewAssetScrapingConfig
    ): Raise[IO, AddScrapingConfigError] ?=> IO[ExistingAssetScrapingConfig] =
      assetService
        .find(scrapingConfig.assetId)
        .flatMap:
          case Some(_) =>
            repository
              .add(scrapingConfig)
              .transact(xa)
              .flatMap:
                case Right(config) => config.pure
                case Left(error)   => error.raise
          case None => AddScrapingConfigError.AssetDoesNotExists.raise

    override def update(
        scrapingConfig: ExistingAssetScrapingConfig
    ): Raise[IO, UpdateScrapingConfigError] ?=> IO[
      ExistingAssetScrapingConfig
    ] =
      assetService
        .find(scrapingConfig.assetId)
        .flatMap:
          case Some(_) =>
            repository
              .update(scrapingConfig)
              .transact(xa)
              .flatMap:
                case Right(config) => config.pure
                case Left(error)   => error.raise
          case None => UpdateScrapingConfigError.AssetDoesNotExists.raise

    override def delete(id: AssetScrapingConfigId): IO[Unit] =
      repository.delete(id).transact(xa)
