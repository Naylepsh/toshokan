package library.author

import cats.syntax.all.*
import http.View.{NavBarItem, dialog, layout}
import library.asset.AssetGroup
import library.author.domain.*
import library.asset.domain.ExistingAsset
import neotype.*
import neotype.interop.cats.given
import scalatags.Text.TypedTag
import scalatags.Text.all.*

class AuthorView(navBarItems: List[NavBarItem]):
  def renderAuthors(authors: List[ExistingAuthor]): TypedTag[String] =
    layout(
      "Authors".some,
      div(
        cls := "mt-5",
        table(
          cls := "table table-zebra",
          thead(
            tr(
              th("Id"),
              th("Name"),
              th()
            )
          ),
          tbody(
            authors.map: author =>
              tr(
                th(author.id.unwrap.show),
                td(author.name),
                td(
                  cls := "text-right",
                  a(
                    cls  := "text-light",
                    href := s"/authors/${author.id}",
                    i(cls := "fa-solid fa-pen-to-square")
                  )
                )
              )
          )
        )
      ),
      navBarItems
    )

  def renderAuthor(
      author: ExistingAuthor,
      groups: List[AssetGroup]
  ): TypedTag[String] =
    val suggestions = groups.filter(_.isMergeSuggestion)
    val singletons  = groups.filterNot(_.isMergeSuggestion).flatMap(_.assets)
    layout(
      author.name.show.some,
      div(
        cls := "mt-5 flex flex-col justify-center w-1/2 mx-auto",
        form(
          cls             := "mb-0",
          attr("hx-put")  := s"/authors/${author.id}",
          attr("hx-ext")  := "json-enc",
          label(
            `for` := "name",
            cls   := "form-control w-full",
            div(cls := "label", span(cls := "label-text", "Name")),
            input(
              cls   := "input input-bordered w-full",
              id    := "name",
              name  := "name",
              value := author.name.show
            )
          ),
          button(
            `type` := "submit",
            cls    := "btn btn-primary w-full mt-3",
            "Submit"
          )
        ),
        a(
          href := s"/author-scraping/authors/${author.id}/configs",
          cls  := "btn btn-secondary mt-3",
          "Scraping configs"
        ),
        if suggestions.nonEmpty then
          div(
            cls := "mt-5",
            h3(cls := "text-lg font-semibold mb-2", s"Merge suggestions (${suggestions.size})"),
            suggestions.map: group =>
              div(
                cls := "card card-bordered mb-3 p-4",
                div(
                  cls := "flex justify-between items-center mb-2",
                  span(cls := "font-mono text-sm opacity-60", group.normalizedTitle),
                  button(
                    cls := "btn btn-warning btn-sm",
                    attr("onclick") := s"openMergePreview('${author.id}', [${group.assets.map(_.id.unwrap).mkString(",")}])",
                    s"Merge ${group.assets.size} assets"
                  )
                ),
                ul(
                  cls := "list-disc list-inside",
                  group.assets.map: asset =>
                    li(
                      a(href := s"/assets/${asset.id}", asset.title),
                      span(cls := "font-mono text-xs opacity-50 ml-1", s"#${asset.id.unwrap}")
                    )
                )
              )
          )
        else div(),
        div(
          cls := "mt-5",
          h3(cls := "text-lg font-semibold mb-2", "Assets"),
          if groups.isEmpty then p("No assets linked to this author.")
          else
            div(
              button(
                cls := "btn btn-warning mb-3",
                attr("onclick") := s"openMergePreview('${author.id}')",
                "Merge selected"
              ),
              dialog(
                id  := "merge_modal",
                cls := "modal",
                div(
                  id  := "merge-modal-content",
                  cls := "modal-box w-11/12 max-w-2xl"
                )
              ),
              table(
                cls := "table table-zebra",
                thead(
                  tr(
                    th(),
                    th("Id"),
                    th("Title")
                  )
                ),
                tbody(
                  (suggestions.flatMap(_.assets) ++ singletons).map: asset =>
                    tr(
                      td(
                        input(
                          `type` := "checkbox",
                          name   := "merge-asset",
                          value  := asset.id.unwrap.show,
                          cls    := "checkbox checkbox-sm"
                        )
                      ),
                      th(asset.id.unwrap.show),
                      td(
                        a(
                          href := s"/assets/${asset.id}",
                          asset.title
                        )
                      )
                    )
                )
              )
            )
        )
      ),
      navBarItems
    )

  def renderMergePreview(
      authorId: AuthorId,
      assets: List[ExistingAsset]
  ): TypedTag[String] =
    val assetIdsJson = assets.map(_.id.unwrap).mkString("[", ",", "]")
    div(
      h3(cls := "text-lg font-semibold mb-3", "Merge assets"),
      p(cls := "mb-3", "Select which asset to keep. All entries from the other assets will be moved into it."),
      div(
        cls := "space-y-2",
        assets.map: asset =>
          label(
            cls := "flex items-center gap-3 cursor-pointer p-2 rounded hover:bg-base-200",
            input(
              `type` := "radio",
              name   := "merge-target",
              value  := asset.id.unwrap.show,
              cls    := "radio radio-sm",
              if asset.id == assets.head.id then checked := "checked" else ()
            ),
            span(cls := "font-mono text-sm", s"#${asset.id.unwrap}"),
            span(asset.title)
          )
      ),
      div(
        cls := "modal-action",
        form(
          method := "dialog",
          button(cls := "btn", "Cancel")
        ),
        button(
          cls := "btn btn-warning",
          attr("onclick") := s"confirmMerge('${authorId}', $assetIdsJson)",
          "Confirm merge"
        )
      )
    )
