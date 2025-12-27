package assetScraping.downloading

import java.net.URI
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.concurrent.duration.FiniteDuration

import assetScraping.downloading.domain.{
  AssetEntryDir,
  BulkDownloadProgress,
  DownloadDir
}
import cats.MonadThrow
import cats.effect.kernel.{Sync, Temporal}
import cats.syntax.all.*
import library.AssetRepository
import library.domain.*
import mangadex.MangadexApi
import neotype.interop.cats.given
import sttp.capabilities.WebSockets
import sttp.client3.*
import sttp.model.Uri

class AssetNotFound(assetId: AssetId)
    extends Exception(s"Asset not found: $assetId")

class NoAssetFoundForEntry(entryId: EntryId)
    extends Exception(s"No asset found for entry=${entryId}")

class AssetHasNoEntry(assetId: AssetId, entryId: EntryId)
    extends Exception(
      s"Asset=${assetId} has no entry=${entryId}"
    )

class UnsupportedUrlForEntryDownloading(url: URI)
    extends Exception(
      s"Unsupported url=${url} for entry downloading"
    )

object NoEntriesApplicableForDownload
    extends Exception("No entries applicable for download")

trait AssetDownloadingService[F[_]]:
  def download(entryId: EntryId): F[AssetEntryDir]
  def downloadAll(assetId: AssetId): F[BulkDownloadProgress]

object AssetDownloadingService:
  case class Config(
      delayBetweenDownloads: FiniteDuration
  )

  def make[F[_]: Sync: Temporal: MonadThrow](
      mangadexApi: MangadexApi[F],
      backend: SttpBackend[F, WebSockets],
      downloadDir: DownloadDir,
      assetRepository: AssetRepository[F],
      config: Config
  ): AssetDownloadingService[F] = new:
    override def downloadAll(assetId: AssetId): F[BulkDownloadProgress] =
      for
        entryGroups <- getEntryGroupsOrderedByNo(assetId)
        initialProgress = BulkDownloadProgress.initial(
          assetId,
          entryGroups.size
        )
        finalProgress <- entryGroups.foldM(initialProgress):
          case (progress, (asset, entryNo, entryGroup)) =>
            downloadEntryGroup(progress, asset, entryNo, entryGroup)
      yield finalProgress

    private def getEntryGroupsOrderedByNo(assetId: AssetId) =
      assetRepository
        .findById(assetId)
        .flatMap:
          case None => Temporal[F].raiseError(AssetNotFound(assetId))
          case Some(asset, entries) =>
            entries
              .groupBy(_.no)
              .toList
              .sortBy(_._1)
              .map((entryNo, entryGroup) => (asset, entryNo, entryGroup))
              .pure

    private def downloadEntryGroup(
        progress: BulkDownloadProgress,
        asset: ExistingAsset,
        entryNo: EntryNo,
        entryGroup: List[ExistingAssetEntry]
    ) =
      for
        _      <- scribe.cats[F].info(s"Downloading entry group ${entryNo}")
        result <- tryDownloadFromGroup(asset, entryGroup).attempt
        _      <- Temporal[F].sleep(config.delayBetweenDownloads)
        updatedProgress <- result match
          case Right(_) =>
            scribe
              .cats[F]
              .info(
                s"Successfully downloaded entry ${entryNo}"
              )
              .as(progress.completedOne)
          case Left(error) =>
            scribe
              .cats[F]
              .error(
                s"Failed to download entry ${entryNo}: ${error.getClass.getSimpleName} - ${error.getMessage}",
                error
              )
              .as(progress.failedOne(entryGroup.head.no))
      yield updatedProgress

    @annotation.tailrec
    private def tryDownloadFromGroup(
        asset: ExistingAsset,
        entryGroup: List[ExistingAssetEntry]
    ): F[AssetEntryDir] =
      entryGroup match
        case Nil =>
          Temporal[F].raiseError(NoEntriesApplicableForDownload)
        case entry :: rest =>
          getImageExtractor(entry) match
            case Some(imageUrlsF) =>
              for
                urls <- imageUrlsF
                dir  <- createDirectory(asset, entry)
                _    <- downloadImages(urls, dir)
              yield dir
            case None =>
              if rest.nonEmpty then tryDownloadFromGroup(asset, rest)
              else
                Temporal[F].raiseError(
                  UnsupportedUrlForEntryDownloading(entry.uri)
                )

    private def getImageExtractor(
        entry: ExistingAssetEntry
    ): Option[F[List[URI]]] =
      entry.uri.toString match
        case s"https://mangadex.org/chapter/$chapterId" =>
          Some(mangadexApi.getImages(chapterId).flatMap(Sync[F].fromEither))
        case _ => None

    override def download(entryId: EntryId): F[AssetEntryDir] =
      for
        (asset, entry) <- findAssetAndEntry(entryId)
        urls           <- getImageUrls(entry)
        dir            <- createDirectory(asset, entry)
        _              <- downloadImages(urls, dir)
      yield dir

    private def findAssetAndEntry(entryId: EntryId) =
      for
        maybeAssetWithEntries <- assetRepository.findByEntryId(entryId)
        (asset, entries) <- Sync[F].fromOption(
          maybeAssetWithEntries,
          NoAssetFoundForEntry(entryId)
        )
        entry <- Sync[F].fromOption(
          entries.find(_.id === entryId),
          AssetHasNoEntry(asset.id, entryId)
        )
      yield (asset, entry)

    private def getImageUrls(entry: ExistingAssetEntry): F[List[URI]] =
      getImageExtractor(entry) match
        case Some(imageUrlsF) => imageUrlsF
        case None =>
          Sync[F].raiseError(UnsupportedUrlForEntryDownloading(entry.uri))

    private def createDirectory(
        asset: ExistingAsset,
        entry: ExistingAssetEntry
    ): F[AssetEntryDir] =
      val dir = AssetEntryDir(downloadDir, asset.title, entry.no)
      Sync[F].blocking(Files.createDirectories(dir)).as(dir)

    private def downloadImages(urls: List[URI], dir: AssetEntryDir): F[Unit] =
      urls.zipWithIndex.traverse_ { (url, index) =>
        val ext = Path
          .of(url.getPath)
          .getFileName
          .toString
          .reverse
          .takeWhile(_ != '.')
          .reverse
        val outputPath = dir.forPage(index + 1, ext)
        downloadImage(url).flatMap: bytes =>
          Sync[F]
            .blocking(
              Files.write(
                outputPath,
                bytes,
                StandardOpenOption.CREATE,
                StandardOpenOption.TRUNCATE_EXISTING
              )
            )
            .as(())
      }

    /** TODO: Add retries
      */
    private def downloadImage(url: URI): F[Array[Byte]] =
      basicRequest
        .get(Uri(url))
        .response(asByteArray)
        .send(backend)
        .map(_.body.leftMap(RuntimeException(_)))
        .flatMap(Sync[F].fromEither)
