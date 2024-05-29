package assetScraping.configs

import cats.effect.kernel.{Clock, Sync}
import cats.syntax.all.*
import library.AssetService
import library.domain.*

import domain.*

trait AssetScrapingConfigService[F[_]]:
  def findAllEnabled: F[List[ExistingAssetScrapingConfig]]
  def findByAssetId(assetId: AssetId): F[Either[
    FindScrapingConfigError,
    (ExistingAsset, List[ExistingAssetScrapingConfig])
  ]]
  def add(
      scrapingConfig: NewAssetScrapingConfig
  ): F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]]
  def update(
      scrapingConfig: ExistingAssetScrapingConfig
  ): F[Either[UpdateScrapingConfigError, ExistingAssetScrapingConfig]]
  def delete(id: AssetScrapingConfigId): F[Unit]

object AssetScrapingService:
  def make[F[_]: Sync: Clock](
      repository: AssetScrapingConfigRepository[F],
      assetService: AssetService[F]
  ): AssetScrapingConfigService[F] = new:

    override def findAllEnabled: F[List[ExistingAssetScrapingConfig]] =
      repository.findAllEnabled

    override def findByAssetId(assetId: AssetId): F[Either[
      FindScrapingConfigError,
      (ExistingAsset, List[ExistingAssetScrapingConfig])
    ]] =
      assetService
        .find(assetId)
        .flatMap:
          case Some(asset, _) =>
            repository
              .findByAssetId(assetId)
              .map: configs =>
                (asset, configs).asRight
          case None => FindScrapingConfigError.AssetDoesNotExists.asLeft.pure

    override def add(
        scrapingConfig: NewAssetScrapingConfig
    ): F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]] =
      assetService
        .find(scrapingConfig.assetId)
        .flatMap:
          case Some(_) => repository.add(scrapingConfig)
          case None    => AddScrapingConfigError.AssetDoesNotExists.asLeft.pure

    override def update(
        scrapingConfig: ExistingAssetScrapingConfig
    ): F[Either[UpdateScrapingConfigError, ExistingAssetScrapingConfig]] =
      assetService
        .find(scrapingConfig.assetId)
        .flatMap:
          case Some(_) => repository.update(scrapingConfig)
          case None => UpdateScrapingConfigError.AssetDoesNotExists.asLeft.pure

    override def delete(id: AssetScrapingConfigId): F[Unit] =
      repository.delete(id)
