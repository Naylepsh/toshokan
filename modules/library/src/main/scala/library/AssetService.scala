package library

import library.domain.{ ExistingAsset, ExistingAssetEntry }

trait AssetService[F[_]]:
  def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]]

object AssetService:
  def make[F[_]](repository: AssetRepository[F]): AssetService[F] = new:
    def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]] =
      repository.findAll
