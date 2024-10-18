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
  def binge(assetId: AssetId): F[Unit]
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

    override def binge(assetId: AssetId): F[Unit] =
      assetService
        .find(assetId)
        .flatMap:
          case None =>
            scribe.cats[F].error(s"No asset with id=${assetId} found")
          case Some(asset, entries) =>
            val updateEntries = entries.traverse(entry =>
              assetService
                .setSeen(asset.id, entry.id, WasEntrySeen(true))
                .map(
                  _.fold(error => scribe.error(error.toString), identity)
                )
                .void
            )
            val updateLatestEntryExternally = pickLatestEntry(entries).fold(
              scribe
                .cats[F]
                .warn(
                  s"Asset ${asset.title} has no entry applicable for updating externally"
                )
            ): no =>
              updateProgressExternally(asset, entries, no)
                .map(_.fold(error => scribe.error(error.getMessage), identity))
            (updateEntries <* updateLatestEntryExternally).void

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

  /** Any EntryNo with fractional number should not be considered latest, as we
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

  def pickLatestEntry(
      entries: List[ExistingAssetEntry]
  ): Option[EntryNo] =
    entries
      .sortBy(_.no.value.toIntOption)(using Ordering[Option[Int]].reverse)
      .pipe(pickLatestEntryInternals)

  /** Assumes that entries are sorted by `entry.no` descending
    */
  @scala.annotation.tailrec
  private def pickLatestEntryInternals(
      entries: List[ExistingAssetEntry]
  ): Option[EntryNo] =
    entries match
      case Nil => None
      case entry :: tail =>
        LatestChapter(entry.no) match
          case None    => pickLatestEntryInternals(tail)
          case Some(_) => entry.no.some
