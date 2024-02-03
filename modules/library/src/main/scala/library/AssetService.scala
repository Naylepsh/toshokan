package library

import cats.syntax.all.*
import library.domain.{ ExistingAsset, ExistingAssetEntry }
import library.domain.AssetId
import cats.Functor

trait AssetService[F[_]]:
  def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]]
  def find(id: AssetId): F[Option[(ExistingAsset, List[ExistingAssetEntry])]]

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
