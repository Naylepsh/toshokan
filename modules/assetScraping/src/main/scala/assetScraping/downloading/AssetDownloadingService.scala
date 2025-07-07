package assetScraping.downloading

import java.net.URI
import java.nio.file.{Files, Path, StandardOpenOption}

import assetScraping.downloading.domain.{AssetEntryDir, DownloadDir}
import cats.effect.kernel.Sync
import cats.syntax.all.*
import library.AssetRepository
import library.domain.*
import mangadex.MangadexApi
import sttp.capabilities.WebSockets
import sttp.client3.*
import sttp.model.Uri

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

trait AssetDownloadingService[F[_]]:
  def download(entryId: EntryId): F[AssetEntryDir]

object AssetDownloadingService:
  def make[F[_]: Sync](
      mangadexApi: MangadexApi[F],
      backend: SttpBackend[F, WebSockets],
      downloadDir: DownloadDir,
      assetRepository: AssetRepository[F]
  ): AssetDownloadingService[F] = new:
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
      entry.uri.value.toString match
        case s"https://mangadex.org/chapter/$chapterId" =>
          mangadexApi.getImages(chapterId).flatMap(Sync[F].fromEither)
        case _ =>
          Sync[F].raiseError(UnsupportedUrlForEntryDownloading(entry.uri))

    private def createDirectory(
        asset: ExistingAsset,
        entry: ExistingAssetEntry
    ): F[AssetEntryDir] =
      val dir = AssetEntryDir(downloadDir, asset.title, entry.no)
      Sync[F].blocking(Files.createDirectories(dir.value)).as(dir)

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
