package externalChapters

import cats.syntax.all.*
import externalChapters.domain.*
import http.View.{NavBarItem, layout}
import scalatags.Text.TypedTag
import scalatags.Text.all.*

class ExternalChapterView(navBarItems: List[NavBarItem]):
  def renderChapters(
      chapters: List[ExternalChapter],
      page: Int,
      totalPages: Int
  ): TypedTag[String] =
    layout(
      "External chapters".some,
      div(
        cls := "mt-5",
        form(
          method := "post",
          action := "/external-chapters/scan",
          button(`type` := "submit", cls := "btn btn-primary", "Scan")
        ),
        div(
          cls := "grid grid-cols-2 md:grid-cols-4 lg:grid-cols-6 gap-4 mt-5",
          chapters.map(chapterCard)
        ),
        div(
          cls := "join flex justify-center mt-5",
          pageLink("«", page - 1, page > 1),
          button(cls := "join-item btn btn-disabled", s"$page / $totalPages"),
          pageLink("»", page + 1, page < totalPages)
        )
      ),
      navBarItems
    )

  private def pageLink(label: String, page: Int, enabled: Boolean) =
    if enabled then
      a(
        cls  := "join-item btn",
        href := s"/external-chapters?page=$page",
        label
      )
    else button(cls := "join-item btn btn-disabled", label)

  private def chapterCard(chapter: ExternalChapter) =
    val thumbnail = chapter.thumbnailExt.map: _ =>
      val image = img(
        cls             := "w-full h-64 object-cover",
        src             := s"/external-chapters/thumbnail/${chapter.id}",
        attr("loading") := "lazy"
      )
      if chapter.missingSince.isEmpty then
        a(href := s"/external-chapters/${chapter.id}/read", image)
      else image
    div(
      cls := "card bg-base-100 shadow",
      thumbnail,
      div(
        cls := "card-body p-2",
        p(cls := "text-sm break-words", chapter.name: String),
        p(cls := "text-xs text-gray-500", s"${chapter.pageCount} pages"),
        chapter.missingSince.map: _ =>
          span(cls := "badge badge-warning", "missing")
      )
    )

  def renderReader(chapter: ExternalChapter): TypedTag[String] =
    layout(
      (chapter.name: String).some,
      div(
        cls := "mt-5 flex flex-col items-center",
        a(
          cls  := "btn btn-ghost self-start",
          href := "/external-chapters",
          "« Back"
        ),
        (0 until chapter.pageCount).map: index =>
          img(
            cls             := "w-full max-w-3xl",
            src             := s"/external-chapters/${chapter.id}/pages/$index",
            attr("loading") := "lazy"
          )
      ),
      navBarItems
    )
