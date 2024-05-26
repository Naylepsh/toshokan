package assetScraping.schedules

import cats.syntax.all.*
import http.View.{NavBarItem, layout}
import library.category.domain.ExistingCategory
import scalatags.Text.TypedTag
import scalatags.Text.all.*

import domain.DayOfTheWeek

class ScheduleView(navBarItems: List[NavBarItem]):
  def renderForm(
      availableCategories: List[ExistingCategory]
  ): TypedTag[String] =
    layout("New schedule".some, formPartial(availableCategories), navBarItems)

  def formPartial(
      availableCategories: List[ExistingCategory]
  ): TypedTag[String] =
    div(
      cls := "mt-5 flex flex-col justify-center w-1/2 mx-auto",
      form(
        id              := "scraping-schedules-form",
        attr("hx-post") := "/scraping-schedules",
        attr("hx-ext")  := "json-enc",
        h2(cls := "text-2xl text-center", "New scraping schedule"),
        categorySelectionPartial(availableCategories),
        minDaysSinceLastScrapePartial,
        daysPartial,
        button(
          `type` := "submit",
          cls    := "btn btn-primary w-full mt-3",
          "Submit"
        )
      )
    )

  private def categorySelectionPartial(categories: List[ExistingCategory]) =
    categories match
      case Nil => div()
      case head :: tail =>
        label(
          cls := "form-control w-full max-w-ws",
          div(
            cls := "label",
            span(cls := "label-text", "Category")
          ),
          select(
            cls  := "select select-bordered",
            name := "categoryId",
            option(
              value    := head.id.value,
              selected := "1",
              head.name.show
            ),
            tail.map: category =>
              option(
                value := category.id.value,
                category.name.show
              )
          )
        )

  private def minDaysSinceLastScrapePartial =
    div(
      cls := "mt-3",
      label(
        cls   := "mr-2 label-text",
        `for` := "minDaysSinceLastScrape",
        "Min. number of days since last scrape"
      ),
      input(
        id          := "minDaysSinceLastScrape",
        cls         := "max-w-16",
        name        := "minDaysSinceLastScrape",
        `type`      := "number",
        value       := 0,
        attr("min") := 0
      )
    )

  private def daysPartial =
    div(
      cls := "mt-3",
      p(cls := "label-text", "Days to scrape on"),
      div(
        cls := "ml-1",
        DayOfTheWeek.fullWeek.toList.map: day =>
          div(
            cls := "flex w-full mb-2",
            input(
              id     := day.name,
              cls    := "mr-2",
              `type` := "checkbox",
              name   := "days",
              value  := day.getValue
            ),
            label(
              cls   := "w-full",
              `for` := day.name,
              day.show
            )
          )
      )
    )
