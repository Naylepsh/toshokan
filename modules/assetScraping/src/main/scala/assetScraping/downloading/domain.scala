package assetScraping.downloading.domain

import java.nio.file.Path

import core.Newt
import library.domain.*

type DownloadDir = DownloadDir.Type
object DownloadDir extends Newt[Path]:
  val tmp: DownloadDir = DownloadDir(java.nio.file.Paths.get("/tmp"))

type AssetEntryDir = AssetEntryDir.Type
object AssetEntryDir extends Newt[Path]:
  def apply(
      downloadDir: DownloadDir,
      assetTitle: AssetTitle,
      entryNo: EntryNo
  ): AssetEntryDir =
    AssetEntryDir(downloadDir.value.resolve(s"${assetTitle} - ${entryNo}"))

  extension (self: AssetEntryDir)
    def forPage(page: Int, extension: String): Path =
      self.value.resolve(s"${page}.${extension}")

case class BulkDownloadProgress(
    assetId: AssetId,
    totalEntries: Int,
    completedEntries: Int,
    failedEntries: List[EntryNo],
    currentEntry: Option[EntryId]
):
  def isComplete: Boolean =
    completedEntries + failedEntries.size == totalEntries

  def successRate: Double =
    if totalEntries == 0 then 1.0 else completedEntries.toDouble / totalEntries

  def completedOne: BulkDownloadProgress =
    copy(completedEntries = completedEntries + 1)

  def failedOne(entryNo: EntryNo): BulkDownloadProgress =
    copy(failedEntries = entryNo :: failedEntries)

object BulkDownloadProgress:
  def initial(
      assetId: AssetId,
      entryCountToDownload: Int
  ): BulkDownloadProgress =
    BulkDownloadProgress(
      assetId,
      entryCountToDownload,
      0,
      List.empty,
      None
    )
