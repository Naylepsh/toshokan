package assetScraping.downloading

import assetScraping.downloading.domain.AssetEntryDir
import cats.syntax.all.*
import http.View.{NavBarItem, layout}
import scalatags.Text.TypedTag
import scalatags.Text.all.*

class AssetDownloadingView(navBarItems: List[NavBarItem]):
  def renderDownloadResult(downloadedTo: AssetEntryDir): TypedTag[String] =
    layout(
      "Download result".some,
      div(
        cls := "mt-5",
        p(s"Entry successfully downloaded to ${downloadedTo.value}")
      ),
      navBarItems
    )
