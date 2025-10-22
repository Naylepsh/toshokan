package assetScraping.configs

import cats.effect.kernel.Sync
import cats.syntax.all.*
import library.AssetService
import library.domain.*
import cats.mtl.Raise
import cats.mtl.syntax.all.*

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

object AssetScrapingService:
  def make[F[_]: Sync](
      repository: AssetScrapingConfigRepository[F],
      assetService: AssetService[F]
  ): AssetScrapingConfigService[F] = new:

    override def findAllEnabled: F[List[ExistingAssetScrapingConfig]] =
      repository.findAllEnabled

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
              .map(asset -> _)
          case None => FindScrapingConfigError.AssetDoesNotExists.raise

    override def add(
        scrapingConfig: NewAssetScrapingConfig
    ): Raise[F, AddScrapingConfigError] ?=> F[ExistingAssetScrapingConfig] =
      assetService
        .find(scrapingConfig.assetId)
        .flatMap:
          case Some(_) => repository.add(scrapingConfig)
          case None    => AddScrapingConfigError.AssetDoesNotExists.raise

    override def update(
        scrapingConfig: ExistingAssetScrapingConfig
    ): Raise[F, UpdateScrapingConfigError] ?=> F[ExistingAssetScrapingConfig] =
      assetService
        .find(scrapingConfig.assetId)
        .flatMap:
          case Some(_) => repository.update(scrapingConfig)
          case None    => UpdateScrapingConfigError.AssetDoesNotExists.raise

    override def delete(id: AssetScrapingConfigId): F[Unit] =
      repository.delete(id)
