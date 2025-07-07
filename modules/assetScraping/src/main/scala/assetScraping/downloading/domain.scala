package assetScraping.downloading.domain

import core.Newt

import java.nio.file.Path
import library.domain.AssetTitle
import library.domain.EntryNo

type DownloadDir = DownloadDir.Type
object DownloadDir extends Newt[Path]

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
