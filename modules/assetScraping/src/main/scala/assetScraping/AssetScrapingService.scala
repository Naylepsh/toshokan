package assetScraping

import cats.Monad
import cats.syntax.all.*
import library.AssetRepository

import domain.*

trait AssetScrapingService[F[_]]:
  def add(scrapingConfig: NewAssetScrapingConfig)
      : F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]]
  def delete(id: AssetScrapingConfigId): F[Unit]

object AssetScrapingService:
  def make[F[_]: Monad](
      repository: AssetScrapingRepository[F],
      assetRepository: AssetRepository[F]
  ): AssetScrapingRepository[F] = new:
    def add(scrapingConfig: NewAssetScrapingConfig)
        : F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]] =
      assetRepository.findById(scrapingConfig.assetId).flatMap:
        case Some(_) => repository.add(scrapingConfig)
        case None    => AddScrapingConfigError.AssetDoesNotExists.asLeft.pure

    def delete(id: AssetScrapingConfigId): F[Unit] =
      repository.delete(id)
