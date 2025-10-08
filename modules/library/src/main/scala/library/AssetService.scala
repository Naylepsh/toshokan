package library

import cats.data.{EitherT, NonEmptyList}
import cats.implicits.*

import domain.*
import domain.Releases.given
import category.domain.CategoryId
import cats.effect.kernel.Sync

trait AssetService[F[_]]:
  def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]]
  def findNotSeenReleases: F[List[Releases]]
  def findStale: F[List[StaleAsset]]
  def find(id: AssetId): F[Option[(ExistingAsset, List[ExistingAssetEntry])]]
  def matchCategoriesToAssets(
      categoryIds: List[CategoryId]
  ): F[Map[CategoryId, List[AssetId]]]
  def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]]
  def add(entry: NewAssetEntry): F[Either[AddEntryError, ExistingAssetEntry]]
  def addIfNewRelease(
      entries: List[NewAssetEntry]
  ): F[List[Either[AddEntryError, List[ExistingAssetEntry]]]]
  def update(asset: ExistingAsset): F[Unit]
  def setSeen(
      assetId: AssetId,
      entryId: EntryId,
      seen: WasEntrySeen
  ): F[Either[UpdateEntryError, (ExistingAsset, ExistingAssetEntry)]]
  def delete(assetId: AssetId): F[Unit]

object AssetService:
  def make[F[_]: Sync](repository: AssetRepository[F]): AssetService[F] =
    new:
      override def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]] =
        repository.findAll

      override def findNotSeenReleases: F[List[Releases]] =
        repository.findAll.map: all =>
          all
            .flatMap: (asset, entries) =>
              entries
                .filter(_.wasSeen.eqv(WasEntrySeen(false)))
                .map(entry => asset -> entry)
            .groupBy: (_, entry) =>
              entry.dateUploaded
            .map: (key, assetsAndEntries) =>
              key -> assetsAndEntries.sortBy(_._1.id)
            .toList
            .sorted(using Ordering[Releases].reverse)

      override def findStale: F[List[StaleAsset]] =
        repository
          .findStale(minDaysToBeStale = 90)
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
      ): F[Either[AddAssetError, ExistingAsset]] =
        repository.add(asset)

      override def add(
          entry: NewAssetEntry
      ): F[Either[AddEntryError, ExistingAssetEntry]] =
        repository.add(entry)

      override def addIfNewRelease(
          entries: List[NewAssetEntry]
      ): F[List[Either[AddEntryError, List[ExistingAssetEntry]]]] =
        entries
          .groupBy(_.assetId)
          .toList
          .traverse: (assetId, entries) =>
            find(assetId).flatMap:
              case None =>
                AddEntryError.AssetDoesNotExists.asLeft.pure
              case Some(_, assetEntries) =>
                val existingUrls = assetEntries.map(_.uri)
                entries
                  .filter(entry => !existingUrls.contains(entry.uri))
                  .traverse(entry => EitherT(add(entry)))
                  .value

      override def update(asset: ExistingAsset): F[Unit] =
        /** TODO: This should probably check whether the asset exists first? But
          * it's not like it's needed for the UI for now
          */
        repository.update(asset)

      override def setSeen(
          assetId: AssetId,
          entryId: EntryId,
          seen: WasEntrySeen
      ): F[Either[UpdateEntryError, (ExistingAsset, ExistingAssetEntry)]] =
        repository
          .findById(assetId)
          .flatMap:
            case None => UpdateEntryError.AssetDoesNotExists.asLeft.pure
            case Some(asset, entries) =>
              entries.find(_.id == entryId) match
                case None => UpdateEntryError.EntryDoesNotExist.asLeft.pure
                case Some(entry) =>
                  val e = entry.copy(wasSeen = seen)
                  repository.update(e).as((asset, e).asRight)

      override def delete(assetId: AssetId): F[Unit] =
        repository.delete(assetId)
