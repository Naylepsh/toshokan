package progressTracking

import assetMapping.AssetMappingService
import cats.data.NonEmptyList
import cats.effect.*
import cats.implicits.*
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import library.AssetService
import library.domain.*
import library.domain.Releases.given
import myAnimeList.MyAnimeListService
import neotype.interop.cats.given

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
      assetMappingService: AssetMappingService[F],
      entryProgressRepository: EntryProgressRepository[F]
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
            entries.find(_.id.eqv(entryId)) match
              case None => UpdateEntryError.EntryDoesNotExist.raise
              case Some(entry) =>
                (
                  entryProgressRepository.setSeen(entryId, wasEntrySeen),
                  if wasEntrySeen
                  then
                    updateProgressExternally(asset, entries, entry.no)
                      .handleErrorWith: error =>
                        scribe.cats[F].error(error.toString).void
                  else Sync[F].unit
                ).mapN((_, _) => (asset, entry))

    override def binge(assetId: AssetId): F[Unit] =
      assetService
        .find(assetId)
        .flatMap:
          case None =>
            scribe.cats[F].error(s"No asset with id=${assetId} found")
          case Some(assetWithEntries) =>
            val (asset, entries) = assetWithEntries
            val updateEntries = entries.traverse: entry =>
              entryProgressRepository
                .setSeen(entry.id, WasEntrySeen(true))
                .handleErrorWith: error =>
                  scribe.cats[F].error(error.toString) *>
                    Sync[F].pure(
                      progressTracking.domain.EntryProgress.notSeen(entry.id)
                    )
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
      // TODO: Make it query assets by entry id instead of all
      for
        seenEntryIds <- entryProgressRepository.findSeenEntries.map(_.toSet)
        allAssets    <- assetService.findAll
        unseenReleases = allAssets
          .flatMap: (asset, _, entries) =>
            entries
              .filterNot(entry => seenEntryIds.contains(entry.id))
              .map(entry => asset -> entry)
          .groupBy: (_, entry) =>
            entry.dateUploaded
          .map: (key, assetsAndEntries) =>
            key -> assetsAndEntries.sortBy(_._1.id)
          .toList
          .sorted(using Ordering[Releases].reverse)
      yield unseenReleases

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
    no.toIntOption
      .map: noNumeric =>
        allEntries
          .mapFilter(_.no.toIntOption)
          .pipe(NonEmptyList.fromList)
          .fold(false)(_.maximum == noNumeric)
      .getOrElse(false)

  def pickLatestEntry(
      entries: List[ExistingAssetEntry]
  ): Option[EntryNo] =
    entries
      .sortBy(_.no.toIntOption)(using Ordering[Option[Int]].reverse)
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
