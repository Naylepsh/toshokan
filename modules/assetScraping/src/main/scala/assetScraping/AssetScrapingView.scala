package assetScraping

import assetScraping.scrapes.domain.ScrapingSummary
import cats.syntax.all.*
import http.View.{NavBarItem, layout}
import library.category.domain.ExistingCategory
import scalatags.Text.TypedTag
import scalatags.Text.all.*

class AssetScrapingView(navBarItems: List[NavBarItem]):
  given Conversion[scalatags.Text.TypedTag[String], String] = _.toString

  def renderScrapingManagement(categories: List[ExistingCategory]): String =
    layout(
      "Asset scraping".some,
      div(
        cls := "max-w-xl mx-auto mt-5",
        div(
          h2(
            cls := "text-xl font-semibold text-center",
            "Scraping Management"
          ),
          a(
            cls  := "btn btn-secondary w-full mt-3",
            href := "/scraping-schedules",
            "Manage scraping schedules"
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
          ),
          categories.map(categoryScrapePartial)
        )
      ),
      navBarItems
    )

  def scrapingSummaryPartial(
      scrapingSummary: ScrapingSummary
  ): TypedTag[String] =
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

  private def categoryScrapePartial(category: ExistingCategory) =
    a(
      cls               := "btn btn-primary w-full mt-3",
      attr("hx-post")   := s"/asset-scraping/category/${category.id}",
      attr("hx-swap")   := "outerHTML",
      attr("hx-target") := "closest div",
      s"Check for ${category.name}'s new releases"
    )
