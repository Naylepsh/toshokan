package assetScraping.configs

import cats.effect.kernel.Sync
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import library.asset.AssetService
import library.asset.domain.*

import domain.*

trait AssetScrapingConfigService[F[_]]:
  def findAllEnabled: F[List[ExistingAssetScrapingConfig]]
  def findByAssetId(assetId: AssetId): Raise[F, FindScrapingConfigError] ?=> F[
    (
        ExistingAsset,
        List[ExistingAssetScrapingConfig]
    )
  ]
  def add(
      scrapingConfig: NewAssetScrapingConfig
  ): Raise[F, AddScrapingConfigError] ?=> F[ExistingAssetScrapingConfig]
  def update(
      scrapingConfig: ExistingAssetScrapingConfig
  ): Raise[F, UpdateScrapingConfigError] ?=> F[ExistingAssetScrapingConfig]
  def delete(id: AssetScrapingConfigId): F[Unit]

object AssetScrapingConfigService:
  def make[F[_]: Sync](
      repository: AssetScrapingConfigRepository,
      assetService: AssetService[F],
      xa: Transactor[F]
  ): AssetScrapingConfigService[F] = new:

    override def findAllEnabled: F[List[ExistingAssetScrapingConfig]] =
      repository.findAllEnabled.transact(xa)

    override def findByAssetId(
        assetId: AssetId
    ): Raise[F, FindScrapingConfigError] ?=> F[
      (
          ExistingAsset,
          List[ExistingAssetScrapingConfig]
      )
    ] =
      assetService
        .find(assetId)
        .flatMap:
          case Some(asset, _) =>
            repository
              .findByAssetId(assetId)
              .transact(xa)
              .map(asset -> _)
          case None => FindScrapingConfigError.AssetDoesNotExists.raise

    override def add(
        scrapingConfig: NewAssetScrapingConfig
    ): Raise[F, AddScrapingConfigError] ?=> F[ExistingAssetScrapingConfig] =
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
    ): Raise[F, UpdateScrapingConfigError] ?=> F[ExistingAssetScrapingConfig] =
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

    override def delete(id: AssetScrapingConfigId): F[Unit] =
      repository.delete(id).transact(xa)
