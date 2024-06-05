package assetScraping.schedules

import cats.syntax.all.*
import http.View.{NavBarItem, layout}
import library.category.domain.{CategoryId, ExistingCategory}
import scalatags.Text.TypedTag
import scalatags.Text.all.*

import domain.DayOfTheWeek
import domain.ScrapingSchedule

class ScheduleView(navBarItems: List[NavBarItem]):
  def renderScheduleLinks(
      categories: List[ExistingCategory]
  ): TypedTag[String] =
    layout(
      "Schedules".some,
      div(
        cls := "mt-5 flex flex-col justify-center w-1/2 mx-auto",
        h2(
          cls := "text-xl font-semibold text-center",
          "Scraping schedules"
        ),
        categories.map: category =>
          a(
            cls  := "btn btn-primary mt-3",
            href := s"/scraping-schedules/${category.id}",
            s"${category.name}'s schedule"
          ),
      ),
      navBarItems
    )

  def renderForm(
      availableCategories: List[ExistingCategory],
      schedule: Option[ScrapingSchedule]
  ): TypedTag[String] =
    val title = inferFormTitle(availableCategories, schedule)
    layout(
      title.some,
      formPartial(availableCategories, schedule, title),
      navBarItems
    )

  def formPartial(
      availableCategories: List[ExistingCategory],
      schedule: Option[ScrapingSchedule],
      title: String
  ): TypedTag[String] =
    div(
      cls := "mt-5 flex flex-col justify-center w-1/2 mx-auto",
      form(
        id := "scraping-schedules-form",
        schedule
          .map(s => attr("hx-put") := s"/scraping-schedules/${s.categoryId}")
          .getOrElse(attr("hx-post") := "/scraping-schedules"),
        attr("hx-ext") := "json-enc",
        h2(cls := "text-2xl text-center", title),
        categorySelectionPartial(
          availableCategories,
          schedule.map(_.categoryId)
        ),
        daysPartial(schedule.map(_.days.toList).getOrElse(List.empty)),
        button(
          `type` := "submit",
          cls    := "btn btn-primary w-full mt-3",
          "Submit"
        )
      )
    )

  private def categorySelectionPartial(
      categories: List[ExistingCategory],
      selectedCategory: Option[CategoryId]
  ) =
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
              value := head.id.value,
              head.name.show,
              inferSelectedModifier(head, selectedCategory)
            ),
            tail.map: category =>
              option(
                value := category.id.value,
                inferSelectedModifier(category, selectedCategory),
                category.name.show
              )
          )
        )

  private def inferSelectedModifier(
      category: ExistingCategory,
      selectedCategory: Option[CategoryId]
  ): Modifier =
    selectedCategory match
      case None =>
        (selected := "1")
      case Some(cat) if cat.eqv(category.id) =>
        (selected := "1")
      case _ =>

  private def daysPartial(selectedDays: List[DayOfTheWeek]) =
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
              value  := day.getValue,
              if selectedDays.contains(day) then checked := "1" else ()
            ),
            label(
              cls   := "w-full",
              `for` := day.name,
              day.show
            )
          )
      )
    )

  private def inferFormTitle(
      availableCategories: List[ExistingCategory],
      schedule: Option[ScrapingSchedule]
  ) =
    schedule
      .map: schedule =>
        val category =
          availableCategories.find(_.id == schedule.categoryId).get
        s"${category.name}'s schedule"
      .getOrElse("New schedule")
