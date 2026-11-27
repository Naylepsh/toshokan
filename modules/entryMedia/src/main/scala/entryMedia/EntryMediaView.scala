package entryMedia

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all.*
import entryMedia.domain.*
import http.View.{NavBarItem, layout}
import scalatags.Text.TypedTag
import scalatags.Text.all.*

class EntryMediaView(navBarItems: List[NavBarItem]):
  def renderMedia(
      mediaList: List[EntryMedia],
      page: Int,
      totalPages: Int,
      query: Option[String]
  ): TypedTag[String] =
    layout(
      "Entry media".some,
      div(
        cls := "mt-5",
        div(
          cls := "flex gap-2",
          form(
            method := "get",
            action := "/entry-media",
            cls    := "flex gap-2 grow",
            input(
              `type`      := "search",
              name        := "q",
              value       := query.getOrElse(""),
              placeholder := "Search by name",
              cls         := "input input-bordered grow"
            ),
            button(`type` := "submit", cls := "btn", "Search")
          ),
          form(
            method := "post",
            action := "/entry-media/scan",
            button(`type` := "submit", cls := "btn btn-primary", "Scan")
          )
        ),
        div(
          cls := "grid grid-cols-2 md:grid-cols-4 lg:grid-cols-6 gap-4 mt-5",
          mediaList.map(mediaCard)
        ),
        div(
          cls := "join flex justify-center mt-5",
          pageLink("«", page - 1, page > 1, query),
          button(cls := "join-item btn btn-disabled", s"$page / $totalPages"),
          pageLink("»", page + 1, page < totalPages, query)
        )
      ),
      navBarItems
    )

  private def pageLink(
      label: String,
      page: Int,
      enabled: Boolean,
      query: Option[String]
  ) =
    val search = query.fold("")(q => s"&q=${URLEncoder.encode(q, UTF_8)}")
    if enabled then
      a(
        cls  := "join-item btn",
        href := s"/entry-media?page=$page$search",
        label
      )
    else button(cls := "join-item btn btn-disabled", label)

  private def mediaCard(media: EntryMedia) =
    val thumbnail = media.thumbnailExt.map: _ =>
      val image = img(
        cls             := "w-full h-64 object-cover",
        src             := s"/entry-media/thumbnail/${media.id}",
        attr("loading") := "lazy"
      )
      if media.missingSince.isEmpty then
        a(href := s"/entry-media/${media.id}/read", image)
      else image
    div(
      cls := "card bg-base-100 shadow",
      thumbnail,
      div(
        cls := "card-body p-2",
        p(cls := "text-sm break-words", media.name: String),
        p(cls := "text-xs text-gray-500", s"${media.pageCount} pages"),
        media.missingSince.map: _ =>
          span(cls := "badge badge-warning", "missing")
      )
    )

  def renderReader(media: EntryMedia): TypedTag[String] =
    layout(
      (media.name: String).some,
      div(
        cls := "mt-5 flex flex-col items-center",
        a(
          cls  := "btn btn-ghost self-start",
          href := "/entry-media",
          "« Back"
        ),
        (0 until media.pageCount).map: index =>
          img(
            cls             := "w-full max-w-3xl",
            src             := s"/entry-media/${media.id}/pages/$index",
            attr("loading") := "lazy"
          )
      ),
      navBarItems
    )
