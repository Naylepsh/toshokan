package library.asset

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.*
import cats.mtl.syntax.all.*
import cats.mtl.{Handle, Raise}
import core.types.PositiveInt
import doobie.*
import doobie.implicits.*
import library.category.domain.{CategoryId, CategoryName}

import domain.*

trait AssetService:
  def findAll: IO[
    List[(ExistingAsset, Option[CategoryName], List[ExistingAssetEntry])]
  ]
  def findStale: IO[List[StaleAsset]]
  def find(id: AssetId): IO[Option[(ExistingAsset, List[ExistingAssetEntry])]]
  def matchCategoriesToAssets(
      categoryIds: List[CategoryId]
  ): IO[Map[CategoryId, List[AssetId]]]
  def add(asset: NewAsset): Raise[IO, AddAssetError] ?=> IO[ExistingAsset]
  def add(
      entry: NewAssetEntry
  ): Raise[IO, AddEntryError] ?=> IO[ExistingAssetEntry]
  def addIfNewRelease(
      entries: List[NewAssetEntry]
  ): IO[List[Either[AddEntryError, ExistingAssetEntry]]]
  def update(asset: ExistingAsset): IO[Unit]
  def delete(assetId: AssetId): IO[Unit]
  def mergeAssets(sourceId: AssetId, targetId: AssetId): IO[Unit]

object AssetService:
  def make(repository: AssetRepository, xa: Transactor[IO]): AssetService =
    new:
      override def findAll =
        repository.findAll.transact(xa)

      override def findStale: IO[List[StaleAsset]] =
        repository
          .findStale(minDaysToBeStale = PositiveInt(90))
          .transact(xa)
          .flatMap: assets =>
            assets.traverse: (asset, lastRelease) =>
              lastRelease
                .traverse(_.daysAgo)
                .map(StaleAsset(asset, lastRelease, _))

      override def find(id: AssetId) =
        repository.findById(id).transact(xa)

      override def matchCategoriesToAssets(categoryIds: List[CategoryId]) =
        NonEmptyList
          .fromList(categoryIds)
          .map(repository.matchCategoriesToAssets(_).transact(xa))
          .getOrElse(IO.pure(Map.empty))

      override def add(
          asset: NewAsset
      ): Raise[IO, AddAssetError] ?=> IO[ExistingAsset] =
        repository
          .add(asset)
          .transact(xa)
          .flatMap:
            case Right(a)    => a.pure
            case Left(error) => error.raise

      override def add(
          entry: NewAssetEntry
      ): Raise[IO, AddEntryError] ?=> IO[ExistingAssetEntry] =
        repository
          .add(entry)
          .transact(xa)
          .flatMap:
            case Right(e)    => e.pure
            case Left(error) => error.raise

      override def addIfNewRelease(entries: List[NewAssetEntry]) =
        entries
          .groupBy(_.assetId)
          .toList
          .traverse: (assetId, entries) =>
            find(assetId).flatMap:
              case None =>
                IO.pure(List(AddEntryError.AssetDoesNotExists.asLeft))
              case Some(_, assetEntries) =>
                val existingUrls = assetEntries.map(_.uri)
                entries
                  .filter(entry => !existingUrls.contains(entry.uri))
                  .traverse: entry =>
                    Handle
                      .allow[AddEntryError](add(entry).map(_.asRight))
                      .rescue(_.asLeft.pure)
          .map(_.flatten)

      override def update(asset: ExistingAsset) =
        repository.update(asset).transact(xa)

      override def delete(assetId: AssetId) =
        repository.delete(assetId).transact(xa)

      override def mergeAssets(sourceId: AssetId, targetId: AssetId) =
        repository.mergeAsset(sourceId, targetId).transact(xa)
