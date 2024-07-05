package progressTracking

import cats.syntax.all.*
import http.View.{NavBarItem, layout}
import library.domain.*
import scalatags.Text.TypedTag
import scalatags.Text.all.*

import domain.Manga

class ProgressTrackingView(navbarItems: List[NavBarItem]):
  def renderMangaSearch(
      mangaTitle: AssetTitle,
      mangaId: AssetId
  ): TypedTag[String] =
    layout(
      mangaTitle.show.some,
      ProgressTrackingView.searchMangaByTerm(mangaId, mangaTitle),
      navbarItems
    )

  def mangaMatchesPartial(matches: List[Manga]): TypedTag[String] =
    div(
      cls := "mx-auto max-w-96",
      table(
        cls := "table table-zebra",
        thead(
          tr(
            th("External Id"),
            th("Title"),
            th("")
          )
        ),
        tbody(
          matches.map: manga =>
            tr(
              th(manga.id.show),
              td(manga.title.show),
              td(
                button(attr("hx-post") := "TODO", "Commit")
              )
            ),
        )
      )
    )

object ProgressTrackingView:
  private def searchMangaByTerm(mangaId: AssetId, mangaTitle: AssetTitle) =
    div(
      cls := "container",
      h2(
        cls := "font-semibold mx-auto mt-3",
        s"""Search for corresponding MAL entry for "${mangaTitle}""""
      ),
      form(
        cls             := "mt-3",
        attr("hx-get")  := s"/progress-tracking/${mangaId.value}",
        attr("hx-swap") := "outerHTML",
        div(
          cls := "flex mx-auto max-w-xl",
          input(
            cls         := "input input-bordered w-full",
            name        := "term",
            placeholder := "Manga term"
          ),
          button(
            `type` := "submit",
            cls    := "btn ml-2",
            "Search"
          )
        )
      )
    )
