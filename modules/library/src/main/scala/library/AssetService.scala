package library

import cats.Functor
import cats.syntax.all.*
import library.domain.*

trait AssetService[F[_]]:
  def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]]
  def find(id: AssetId): F[Option[(ExistingAsset, List[ExistingAssetEntry])]]
  def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]]
  def delete(assetId: AssetId): F[Unit]

object AssetService:
  def make[F[_]: Functor](repository: AssetRepository[F]): AssetService[F] =
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

      def delete(assetId: AssetId): F[Unit] =
        repository.delete(assetId)
