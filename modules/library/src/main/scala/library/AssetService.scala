package library

import cats.Functor
import cats.syntax.all.*
import io.circe.Decoder
import io.github.arainko.ducktape.*
import library.domain.*
import cats.data.EitherT
import cats.Applicative
import cats.Monad

case class NewScrapingConfig(
    isEnabled: IsConfigEnabled,
    site: Site,
    uri: ScrapingConfigUri
) derives Decoder:
  def attach(assetId: AssetId): NewAssetScrapingConfig =
    this.into[NewAssetScrapingConfig].transform(Field.const(_.assetId, assetId))

trait AssetService[F[_]]:
  def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]]
  def find(id: AssetId): F[Option[(ExistingAsset, List[ExistingAssetEntry])]]
  def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]]
  def add(asset: NewAsset, configs: List[NewScrapingConfig]): F[Either[
    AddAssetError | AddScrapingConfigError,
    (ExistingAsset, List[ExistingAssetScrapingConfig])
  ]]
  def update(asset: ExistingAsset): F[Unit]
  def delete(assetId: AssetId): F[Unit]

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

      def add(asset: NewAsset, configs: List[NewScrapingConfig]): F[Either[
        AddAssetError | AddScrapingConfigError,
        (ExistingAsset, List[ExistingAssetScrapingConfig])
      ]] =
        repository.add(asset).flatMap:
          case Left(reason) => reason.asLeft.pure
          case Right(asset) =>
            configs
              .map: config =>
                EitherT(repository.add(config.attach(asset.id)))
              .sequence
              .value
              .flatMap:
                case Left(reason) =>
                  repository.delete(asset.id).as(reason.asLeft)
                case Right(configs) => (asset, configs).asRight.pure

      def update(asset: ExistingAsset): F[Unit] =
        /**
         * TODO: This should probably check whether the asset exists first?
         * But it's not like it's needed for the UI for now
         */
        repository.update(asset)

      def delete(assetId: AssetId): F[Unit] =
        repository.delete(assetId)
