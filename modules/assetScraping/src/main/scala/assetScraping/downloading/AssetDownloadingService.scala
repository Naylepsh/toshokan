package assetScraping.downloading

import java.net.URI
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import assetScraping.downloading.domain.{
  AssetEntryDir,
  BulkDownloadProgress,
  DownloadDir
}
import cats.effect.IO
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import library.asset.AssetRepository
import library.asset.domain.*
import mangadex.MangadexApi
import neotype.interop.cats.given
import retry.{ResultHandler, RetryPolicies, retryingOnErrors}
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

trait AssetDownloadingService[FolderCreated]:
  def download(entryId: EntryId): IO[FolderCreated]
  def downloadAll(assetId: AssetId): IO[BulkDownloadProgress]

object AssetDownloadingService:
  case class Config(
      delayBetweenDownloads: FiniteDuration
  )

  def make[FolderCreated](
      mangadexApi: MangadexApi,
      backend: SttpBackend[IO, WebSockets],
      assetRepository: AssetRepository,
      storage: EntryStorage[FolderCreated],
      config: Config,
      xa: Transactor[IO]
  ): AssetDownloadingService[FolderCreated] = new:
    override def downloadAll(assetId: AssetId): IO[BulkDownloadProgress] =
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
        .transact(xa)
        .flatMap:
          case None => IO.raiseError(AssetNotFound(assetId))
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
        _      <- scribe.cats[IO].info(s"Downloading entry group ${entryNo}")
        result <- tryDownloadFromGroup(asset, entryGroup).attempt
        _      <- IO.sleep(config.delayBetweenDownloads)
        updatedProgress <- result match
          case Right(_) =>
            scribe
              .cats[IO]
              .info(
                s"Successfully downloaded entry ${entryNo}"
              )
              .as(progress.completedOne)
          case Left(error) =>
            scribe
              .cats[IO]
              .error(
                s"Failed to download entry ${entryNo}: ${error.getMessage}",
                error
              )
              .as(progress.failedOne(entryGroup.head.no))
      yield updatedProgress

    @annotation.tailrec
    private def tryDownloadFromGroup(
        asset: ExistingAsset,
        entryGroup: List[ExistingAssetEntry]
    ): IO[Unit] =
      entryGroup match
        case Nil =>
          IO.raiseError(NoEntriesApplicableForDownload)
        case entry :: rest =>
          getImageExtractor(entry) match
            case Some(imageUrlsF) =>
              for
                urls   <- imageUrlsF
                folder <- storage.createFolder(asset, entry)
                _      <- downloadImages(urls, folder)
              yield ()
            case None =>
              if rest.nonEmpty then tryDownloadFromGroup(asset, rest)
              else
                IO.raiseError(
                  UnsupportedUrlForEntryDownloading(entry.uri)
                )

    private def getImageExtractor(
        entry: ExistingAssetEntry
    ): Option[IO[List[URI]]] =
      entry.uri.toString match
        case s"https://mangadex.org/chapter/$chapterId" =>
          Some(mangadexApi.getImages(chapterId).flatMap(IO.fromEither))
        case _ => None

    override def download(entryId: EntryId): IO[FolderCreated] =
      for
        (asset, entry) <- findAssetAndEntry(entryId)
        urls           <- getImageUrls(entry)
        dir            <- storage.createFolder(asset, entry)
        _              <- downloadImages(urls, dir)
      yield dir

    private def findAssetAndEntry(entryId: EntryId) =
      for
        maybeAssetWithEntries <- assetRepository
          .findByEntryId(entryId)
          .transact(xa)
        (asset, entries) <- maybeAssetWithEntries.liftTo[IO](
          NoAssetFoundForEntry(entryId)
        )
        entry <- entries
          .find(_.id === entryId)
          .liftTo[IO](AssetHasNoEntry(asset.id, entryId))
      yield (asset, entry)

    private def getImageUrls(entry: ExistingAssetEntry): IO[List[URI]] =
      getImageExtractor(entry) match
        case Some(imageUrlsF) => imageUrlsF
        case None =>
          IO.raiseError(UnsupportedUrlForEntryDownloading(entry.uri))

    private def downloadImages(
        urls: List[URI],
        folderCreated: FolderCreated
    ): IO[Unit] =
      urls.zipWithIndex.traverse_ { (url, index) =>
        val ext = Path
          .of(url.getPath)
          .getFileName
          .toString
          .reverse
          .takeWhile(_ != '.')
          .reverse
        for
          bytes <- downloadImage(url)
          _     <- storage.addEntryPage(folderCreated, index + 1, ext, bytes)
        yield ()
      }

    private def downloadImage(url: URI): IO[Array[Byte]] =
      val request = basicRequest
        .get(Uri(url))
        .response(asByteArray)
        .send(backend)
        .map(_.body.leftMap(RuntimeException(_)))
        .flatMap(IO.fromEither)
      retryingOnErrors(request)(
        policy = RetryPolicies
          .limitRetries[IO](2)
          .join(RetryPolicies.exponentialBackoff[IO](1.second)),
        errorHandler = ResultHandler.retryOnAllErrors(ResultHandler.noop)
      )

trait EntryStorage[CreatedFolder]:
  def createFolder(
      asset: ExistingAsset,
      entry: ExistingAssetEntry
  ): IO[CreatedFolder]
  def addEntryPage(
      folder: CreatedFolder,
      page: Int,
      ext: String,
      bytes: Array[Byte]
  ): IO[Unit]

class EntryLocalStorage(downloadDir: DownloadDir)
    extends EntryStorage[AssetEntryDir]:
  def createFolder(
      asset: ExistingAsset,
      entry: ExistingAssetEntry
  ): IO[AssetEntryDir] =
    val dir = AssetEntryDir(downloadDir, asset.title, entry.no)
    IO.blocking(Files.createDirectories(dir)).as(dir)

  def addEntryPage(
      folder: AssetEntryDir,
      page: Int,
      ext: String,
      bytes: Array[Byte]
  ): IO[Unit] =
    val outputPath = folder.forPage(page, ext)
    IO.blocking(
      Files.write(
        outputPath,
        bytes,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
    ).void
