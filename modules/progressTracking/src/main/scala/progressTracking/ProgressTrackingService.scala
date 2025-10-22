package progressTracking

import assetMapping.AssetMappingService
import cats.data.NonEmptyList
import cats.effect.*
import cats.mtl.syntax.all.*
import cats.mtl.{Handle, Raise}
import cats.syntax.all.*
import library.AssetService
import library.domain.*
import myAnimeList.MyAnimeListService

import util.chaining.*
import domain.*

trait ProgressTrackingService[F[_]]:
  def updateProgress(
      assetId: AssetId,
      entryId: EntryId,
      wasEntrySeen: WasEntrySeen
  ): Raise[F, UpdateEntryError] ?=> F[(ExistingAsset, ExistingAssetEntry)]
  def binge(assetId: AssetId): F[Unit]
  def findNotSeenReleases: F[List[Releases]]

object ProgressTrackingService:
  def make[F[_]: Sync](
      malService: MyAnimeListService[F],
      assetService: AssetService[F],
      assetMappingService: AssetMappingService[F]
  ): ProgressTrackingService[F] = new:
    override def updateProgress(
        assetId: AssetId,
        entryId: EntryId,
        wasEntrySeen: WasEntrySeen
    ): Raise[F, UpdateEntryError] ?=> F[(ExistingAsset, ExistingAssetEntry)] =
      assetService
        .find(assetId)
        .flatMap:
          case None => UpdateEntryError.AssetDoesNotExists.raise
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
                    updateProgressExternally(asset, entries, entry.no)
                      .handleErrorWith: error =>
                        scribe.cats[F].error(error.toString).void
              else Sync[F].unit
            ).mapN((a, _) => a)

    override def binge(assetId: AssetId): F[Unit] =
      assetService
        .find(assetId)
        .flatMap:
          case None =>
            scribe.cats[F].error(s"No asset with id=${assetId} found")
          case Some(asset, entries) =>
            val updateEntries = entries.traverse: entry =>
              Handle
                .allow[UpdateEntryError]:
                  assetService
                    .setSeen(asset.id, entry.id, WasEntrySeen(true))
                    .void
                .rescue:
                  case error => scribe.cats[F].error(error.toString)
            val updateLatestEntryExternally = pickLatestEntry(entries).fold(
              scribe
                .cats[F]
                .warn(
                  s"Asset ${asset.title} has no entry applicable for updating externally"
                )
            ): no =>
              updateProgressExternally(asset, entries, no)
                .handleError(error => scribe.error(error.getMessage))
            (updateEntries <* updateLatestEntryExternally).void

    override def findNotSeenReleases: F[List[Releases]] =
      // TODO: Move the AssetService's logic here and remove this method from it?
      assetService.findNotSeenReleases

    private def updateProgressExternally(
        asset: ExistingAsset,
        entries: List[ExistingAssetEntry],
        no: EntryNo
    ): F[Unit] =
      (isLatestEntry(no, entries), no.asLatestChapter) match
        case (true, Some(latestChapter)) =>
          assetMappingService
            .findExternalId(asset)
            .flatMap:
              case None => ().pure
              case Some(externalId) =>
                malService.updateProgress(externalId, latestChapter)
        case _ => ().pure

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
        entry.no.asLatestChapter match
          case None    => pickLatestEntryInternals(tail)
          case Some(_) => entry.no.some
