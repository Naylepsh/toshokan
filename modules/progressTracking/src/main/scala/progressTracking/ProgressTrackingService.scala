package progressTracking

import cats.Parallel
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*
import doobie.util.transactor.Transactor
import library.AssetService
import library.category.CategoryService
import library.domain.*
import progressTracking.mal.{Manga as _, *}

import util.chaining.*
import domain.*
import assetMapping.AssetMappingService

trait ProgressTrackingService[F[_]]:
  def updateProgress(
      assetId: AssetId,
      entryId: EntryId,
      wasEntrySeen: WasEntrySeen
  ): F[Either[UpdateEntryError, (ExistingAsset, ExistingAssetEntry)]]
  def findNotSeenReleases: F[List[Releases]]

object ProgressTrackingService:
  def make[F[_]: Sync: Parallel](
      xa: Transactor[F],
      malService: MyAnimeListService[F],
      assetService: AssetService[F],
      assetMappingService: AssetMappingService[F],
      categoryService: CategoryService[F]
  ): ProgressTrackingService[F] = new:
    override def updateProgress(
        assetId: AssetId,
        entryId: EntryId,
        wasEntrySeen: WasEntrySeen
    ): F[Either[UpdateEntryError, (ExistingAsset, ExistingAssetEntry)]] =
      assetService
        .find(assetId)
        .flatMap:
          case None => UpdateEntryError.AssetDoesNotExists.asLeft.pure
          case Some(asset, entries) =>
            (
              // TODO: entry management (setSeen) should be moved to progress tracking?
              // Probably as a separate database entity (separate from entry)
              assetService.setSeen(assetId, entryId, wasEntrySeen),
              if wasEntrySeen
              then
                entries
                  .find(_.id.eqv(entryId))
                  .fold(Sync[F].unit): entry =>
                    updateProgressExternally(asset, entries, entry.no).flatMap:
                      result =>
                        result.fold(
                          error => scribe.cats[F].error(error.toString),
                          _ => Sync[F].unit
                        )
              else Sync[F].unit
            ).mapN((a, _) => a)

    override def findNotSeenReleases: F[List[Releases]] =
      // TODO: Move the AssetService's logic here and remove this method from it?
      assetService.findNotSeenReleases

    private def updateProgressExternally(
        asset: ExistingAsset,
        entries: List[ExistingAssetEntry],
        no: EntryNo
    ): F[Either[Throwable, Unit]] =
      (isLatestEntry(no, entries), LatestChapter(no)) match
        case (true, Some(latestChapter)) =>
          assetMappingService
            .findExternalId(asset)
            .flatMap(_.traverse(malService.updateProgress(_, latestChapter)))
            .as(Either.unit)
        case _ => Either.unit.pure

  /** EntryNo can be just a number, but it can also be a full chapter title. It
    * would probably make sense to force a split into separate EntryTitle and
    * EntryNo (which could default to 1 in the case of extraction failure)
    *
    * Any EntryNo with fractional number should not be considered latest, as we
    * don't known whether there are any more entries within the same integer
    * mark
    */
  def isLatestEntry(
      no: EntryNo,
      allEntries: List[ExistingAssetEntry]
  ): Boolean =
    no.value.toIntOption
      .map: noNumeric =>
        allEntries
          .mapFilter(_.no.value.toIntOption)
          .pipe(NonEmptyList.fromList)
          .fold(false)(_.maximum == noNumeric)
      .getOrElse(false)
