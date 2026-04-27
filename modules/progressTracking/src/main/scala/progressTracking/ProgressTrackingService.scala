package progressTracking

import assetMapping.AssetMappingService
import cats.effect.IO
import cats.implicits.*
import cats.mtl.Raise
import core.syntax.*
import doobie.*
import doobie.implicits.*
import library.asset.AssetService
import library.asset.domain.*
import library.asset.domain.Releases.given
import library.author.AuthorRepository
import library.author.domain.AuthorName
import myAnimeList.MyAnimeListService
import neotype.interop.cats.given

import domain.*

trait ProgressTrackingService:
  def updateProgress(
      assetId: AssetId,
      entryId: EntryId,
      wasEntrySeen: WasEntrySeen
  ): Raise[IO, UpdateEntryError] ?=> IO[
    (ExistingAsset, ExistingAssetEntry, Set[AuthorName])
  ]
  def binge(assetId: AssetId): IO[Unit]
  def findNotSeenReleases: IO[List[Releases]]

object ProgressTrackingService:
  def make(
      malService: MyAnimeListService,
      assetService: AssetService,
      assetMappingService: AssetMappingService,
      entryProgressRepository: EntryProgressRepository,
      authorRepository: AuthorRepository,
      xa: Transactor[IO]
  ): ProgressTrackingService = new:
    override def updateProgress(
        assetId: AssetId,
        entryId: EntryId,
        wasEntrySeen: WasEntrySeen
    ): Raise[IO, UpdateEntryError] ?=> IO[
      (ExistingAsset, ExistingAssetEntry, Set[AuthorName])
    ] =
      for
        result <- assetService.find(assetId)
        (asset, entries) <- result.orRaise(UpdateEntryError.AssetDoesNotExists)
        entry <- entries.find(_.id.eqv(entryId)).orRaise(UpdateEntryError.EntryDoesNotExist)
        authors <- authorRepository.findByIds(asset.authors).transact(xa)
        _ <- entryProgressRepository.setSeen(entryId, wasEntrySeen).transact(xa)
        _ <-
          updateProgressExternally(asset, entries, entry.no)
            .whenA(wasEntrySeen)
            .handleErrorWith: error =>
              scribe.cats[IO].error(error.toString).void
      yield (asset, entry, authors.map(_.name).toSet)

    override def binge(assetId: AssetId): IO[Unit] =
      assetService
        .find(assetId)
        .flatMap:
          case None =>
            scribe.cats[IO].error(s"No asset with id=${assetId} found")
          case Some(assetWithEntries) =>
            val (asset, entries) = assetWithEntries
            val updateEntries = entries.traverse: entry =>
              entryProgressRepository
                .setSeen(entry.id, WasEntrySeen(true))
                .transact(xa)
                .handleErrorWith: error =>
                  scribe.cats[IO].error(error.toString) *>
                    IO.pure(
                      progressTracking.domain.EntryProgress.notSeen(entry.id)
                    )
            val updateLatestEntryExternally = ExistingAssetEntry
              .pickLatestEntry(entries)
              .fold(
                scribe
                  .cats[IO]
                  .warn(
                    s"Asset ${asset.title} has no entry applicable for updating externally"
                  )
              ): no =>
                updateProgressExternally(asset, entries, no)
                  .handleError(error => scribe.error(error.getMessage))
            (updateEntries <* updateLatestEntryExternally).void

    override def findNotSeenReleases: IO[List[Releases]] =
      for
        seenEntryIds <- entryProgressRepository.findSeenEntries
          .transact(xa)
          .map(_.toSet)
        allAssets <- assetService.findAll
        authors <- authorRepository.findAll
          .transact(xa)
          .map: authors =>
            authors.map(author => author.id -> author.name).toMap
        unseenReleases = allAssets
          .flatMap: (asset, _, entries) =>
            val authorsOfThis = asset.authors.flatMap(authors.get).toSet
            entries
              .filterNot(entry => seenEntryIds.contains(entry.id))
              .map(entry => (asset, entry, authorsOfThis))
          .groupBy: (_, entry, _) =>
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
    ): IO[Unit] =
      (ExistingAssetEntry.isLatestEntry(no, entries), no.asLatestChapter) match
        case (true, Some(latestChapter)) =>
          assetMappingService
            .findExternalId(asset)
            .flatMap:
              case None => ().pure
              case Some(externalId) =>
                malService.updateProgress(externalId, latestChapter)
        case _ => ().pure
