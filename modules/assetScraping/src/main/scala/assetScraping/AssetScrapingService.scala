package assetScraping

import cats.Monad
import cats.syntax.all.*
import library.AssetService
import library.domain.AssetId

import domain.*

trait AssetScrapingService[F[_]]:
  def findByAssetId(assetId: AssetId)
      : F[Either[FindScrapingConfigError, List[ExistingAssetScrapingConfig]]]
  def add(scrapingConfig: NewAssetScrapingConfig)
      : F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]]
  def delete(id: AssetScrapingConfigId): F[Unit]

object AssetScrapingService:
  def make[F[_]: Monad](
      repository: AssetScrapingRepository[F],
      assetService: AssetService[F]
  ): AssetScrapingService[F] = new:
    def findByAssetId(assetId: AssetId): F[Either[
      FindScrapingConfigError,
      List[ExistingAssetScrapingConfig]
    ]] =
      assetService.find(assetId).flatMap:
        case Some(_) => List.empty.asRight.pure // TODO
        case None    => FindScrapingConfigError.AssetDoesNotExists.asLeft.pure

    def add(scrapingConfig: NewAssetScrapingConfig)
        : F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]] =
      assetService.find(scrapingConfig.assetId).flatMap:
        case Some(_) => repository.add(scrapingConfig)
        case None    => AddScrapingConfigError.AssetDoesNotExists.asLeft.pure

    def delete(id: AssetScrapingConfigId): F[Unit] =
      repository.delete(id)
