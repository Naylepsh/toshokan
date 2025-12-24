package assetScraping.downloading

import assetScraping.downloading.domain.{AssetEntryDir, BulkDownloadProgress}
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

  def renderBulkDownloadResult(
      progress: BulkDownloadProgress
  ): TypedTag[String] =
    div(
      div(
        cls := "card bg-base-100 shadow-xl",
        div(
          cls := "card-body",
          h2(cls := "card-title", "Bulk Download Complete"),
          div(
            cls := "stats stats-vertical lg:stats-horizontal shadow",
            div(
              cls := "stat",
              div(cls := "stat-title", "Total Entries"),
              div(cls := "stat-value", progress.totalEntries.toString)
            ),
            div(
              cls := "stat",
              div(cls := "stat-title", "Downloaded"),
              div(
                cls := "stat-value text-success",
                progress.completedEntries.toString
              )
            ),
            div(
              cls := "stat",
              div(cls := "stat-title", "Failed"),
              div(
                cls := "stat-value text-error",
                progress.failedEntries.size.toString
              )
            ),
            div(
              cls := "stat",
              div(cls := "stat-title", "Success Rate"),
              div(cls := "stat-value", f"${progress.successRate * 100}%.1f%%")
            )
          ),
          if progress.failedEntries.nonEmpty then
            div(
              cls := "mt-4",
              h3(cls := "text-lg font-semibold", "Failed Downloads:"),
              ul(
                cls := "list-disc list-inside",
                progress.failedEntries.map(entryId =>
                  li(s"Entry ID: ${entryId.value}")
                )
              )
            )
          else div(),
          div(
            cls := "modal-action",
            button(
              cls             := "btn",
              attr("onclick") := "bulk_download_modal.close()",
              "Close"
            )
          )
        )
      )
    )
