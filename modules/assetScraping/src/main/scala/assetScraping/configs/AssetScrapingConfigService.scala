package assetScraping.configs

import cats.effect.IO
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import core.syntax.*
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
      for
        (asset, _) <- assetService
          .find(assetId)
          .someOrRaise(FindScrapingConfigError.AssetDoesNotExists)
        configs <- repository.findByAssetId(assetId).transact(xa)
      yield (asset, configs)

    override def add(
        scrapingConfig: NewAssetScrapingConfig
    ): Raise[IO, AddScrapingConfigError] ?=> IO[ExistingAssetScrapingConfig] =
      for
        _ <- assetService
          .find(scrapingConfig.assetId)
          .someOrRaise(AddScrapingConfigError.AssetDoesNotExists)
        result <- repository
          .add(scrapingConfig)
          .transact(xa)
          .flatMap:
            case Right(config) => config.pure
            case Left(error)   => error.raise
      yield result

    override def update(
        scrapingConfig: ExistingAssetScrapingConfig
    ): Raise[IO, UpdateScrapingConfigError] ?=> IO[
      ExistingAssetScrapingConfig
    ] =
      for
        _ <- assetService
          .find(scrapingConfig.assetId)
          .someOrRaise(UpdateScrapingConfigError.AssetDoesNotExists)
        result <- repository
          .update(scrapingConfig)
          .transact(xa)
          .flatMap:
            case Right(config) => config.pure
            case Left(error)   => error.raise
      yield result

    override def delete(id: AssetScrapingConfigId): IO[Unit] =
      repository.delete(id).transact(xa)
