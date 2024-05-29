package assetScraping

import cats.syntax.all.*
import http.View.{layout}
import scalatags.Text.all.*

import http.View.NavBarItem
import assetScraping.scrapes.domain.ScrapingSummary

class AssetScrapingView(navBarItems: List[NavBarItem]):
  given Conversion[scalatags.Text.TypedTag[String], String] = _.toString

  def renderScrapingManagement: String =
    layout(
      "Asset scraping".some,
      div(
        cls := "max-w-xl mx-auto mt-5",
        div(
          h2(
            cls := "text-xl font-semibold text-center",
            "Trigger Asset Scraping"
          ),
          a(
            cls               := "btn btn-primary w-full mt-3",
            attr("hx-post")   := "/asset-scraping?scrape-type=full",
            attr("hx-swap")   := "outerHTML",
            attr("hx-target") := "closest div",
            "Check for all new releases"
          ),
          a(
            cls               := "btn btn-primary w-full mt-3",
            attr("hx-post")   := "/asset-scraping?scrape-type=schedule-only",
            attr("hx-swap")   := "outerHTML",
            attr("hx-target") := "closest div",
            "Check for scheduled new releases"
          )
        )
      ),
      navBarItems
    )

  def scrapingSummaryPartial(scrapingSummary: ScrapingSummary) =
    div(
      cls := "mx-auto max-w-96",
      table(
        cls := "table table-zebra",
        thead(
          tr(
            th("Metric"),
            th("Value")
          )
        ),
        tbody(
          tr(
            th("New entries count"),
            td(scrapingSummary.newEntriesCount)
          ),
          tr(
            th("Used configs count"),
            td(scrapingSummary.configsCount)
          ),
          tr(
            th("Errors count"),
            td(scrapingSummary.errorsCount)
          ),
          tr(
            th("Scraping time"),
            td(s"${scrapingSummary.scrapingTimeSeconds}s")
          ),
          tr(
            th("Saving time"),
            td(s"${scrapingSummary.savingTimeSeconds}s")
          )
        )
      )
    )
