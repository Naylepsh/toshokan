package library

import cats.Monad
import cats.syntax.all.*
import io.circe.Decoder
import library.domain.*

trait AssetService[F[_]]:
  def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]]
  def find(id: AssetId): F[Option[(ExistingAsset, List[ExistingAssetEntry])]]
  def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]]
  def add(config: NewAssetScrapingConfig): F[Either[
    AddScrapingConfigError,
    ExistingAssetScrapingConfig
  ]]
  def update(asset: ExistingAsset): F[Unit]
  def delete(assetId: AssetId): F[Unit]
  def deleteScrapingConfig(scrapingConfigId: AssetScrapingConfigId): F[Unit]

object AssetService:
  def make[F[_]: Monad](repository: AssetRepository[F]): AssetService[F] =
    new:
      def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]] =
        repository.findAll

      def find(id: AssetId)
          : F[Option[(ExistingAsset, List[ExistingAssetEntry])]] =
        // TODO: Add a way to find only 1 asset to AssetRepository
        findAll.map: all =>
          all.find: (asset, _) =>
            asset.id == id

      def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]] =
        repository.add(asset)

      def add(config: NewAssetScrapingConfig)
          : F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]] =
        repository.add(config)

      def update(asset: ExistingAsset): F[Unit] =
        /**
         * TODO: This should probably check whether the asset exists first?
         * But it's not like it's needed for the UI for now
         */
        repository.update(asset)

      def delete(assetId: AssetId): F[Unit] =
        repository.delete(assetId)

      def deleteScrapingConfig(scrapingConfigId: AssetScrapingConfigId)
          : F[Unit] =
        repository.deleteScrapingConfig(scrapingConfigId)
