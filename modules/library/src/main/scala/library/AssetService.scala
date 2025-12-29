package library

import cats.data.NonEmptyList
import cats.effect.kernel.Sync
import cats.implicits.*
import cats.mtl.{Handle, Raise}
import core.types.PositiveInt

import domain.*
import category.domain.{CategoryId, CategoryName}

trait AssetService[F[_]]:
  def findAll
      : F[List[(ExistingAsset, Option[CategoryName], List[ExistingAssetEntry])]]
  def findStale: F[List[StaleAsset]]
  def find(id: AssetId): F[Option[(ExistingAsset, List[ExistingAssetEntry])]]
  def matchCategoriesToAssets(
      categoryIds: List[CategoryId]
  ): F[Map[CategoryId, List[AssetId]]]
  def add(asset: NewAsset): Raise[F, AddAssetError] ?=> F[ExistingAsset]
  def add(
      entry: NewAssetEntry
  ): Raise[F, AddEntryError] ?=> F[ExistingAssetEntry]
  def addIfNewRelease(
      entries: List[NewAssetEntry]
  ): F[List[Either[AddEntryError, ExistingAssetEntry]]]
  def update(asset: ExistingAsset): F[Unit]
  def delete(assetId: AssetId): F[Unit]

object AssetService:
  def make[F[_]: Sync](repository: AssetRepository[F]): AssetService[F] =
    new:
      override def findAll: F[
        List[(ExistingAsset, Option[CategoryName], List[ExistingAssetEntry])]
      ] =
        repository.findAll

      override def findStale: F[List[StaleAsset]] =
        repository
          .findStale(minDaysToBeStale = PositiveInt(90))
          .flatMap: assets =>
            assets.traverse: (asset, lastRelease) =>
              lastRelease.daysAgo[F].map(StaleAsset(asset, lastRelease, _))

      override def find(
          id: AssetId
      ): F[Option[(ExistingAsset, List[ExistingAssetEntry])]] =
        repository.findById(id)

      override def matchCategoriesToAssets(
          categoryIds: List[CategoryId]
      ): F[Map[CategoryId, List[AssetId]]] =
        NonEmptyList
          .fromList(categoryIds)
          .map(repository.matchCategoriesToAssets)
          .getOrElse(Map.empty.pure)

      override def add(
          asset: NewAsset
      ): Raise[F, AddAssetError] ?=> F[ExistingAsset] =
        repository.add(asset)

      override def add(
          entry: NewAssetEntry
      ): Raise[F, AddEntryError] ?=> F[ExistingAssetEntry] =
        repository.add(entry)

      override def addIfNewRelease(
          entries: List[NewAssetEntry]
      ): F[List[Either[AddEntryError, ExistingAssetEntry]]] =
        entries
          .groupBy(_.assetId)
          .toList
          .traverse: (assetId, entries) =>
            find(assetId).flatMap:
              case None =>
                List(AddEntryError.AssetDoesNotExists.asLeft).pure
              case Some(_, assetEntries) =>
                val existingUrls = assetEntries.map(_.uri)
                entries
                  .filter(entry => !existingUrls.contains(entry.uri))
                  .traverse: entry =>
                    Handle
                      .allow[AddEntryError](add(entry).map(_.asRight))
                      .rescue(_.asLeft.pure)
          .map(_.flatten)

      override def update(asset: ExistingAsset): F[Unit] =
        repository.update(asset)

      override def delete(assetId: AssetId): F[Unit] =
        repository.delete(assetId)
