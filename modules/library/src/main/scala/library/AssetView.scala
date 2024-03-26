package library

import cats.syntax.all.*
import library.domain.*
import scalatags.Text.all.*
import http.View.layout

object AssetView:
  given Conversion[scalatags.Text.TypedTag[String], String] = _.toString

  def renderAssets(assetsViewEntries: List[(
      ExistingAsset,
      List[ExistingAssetEntry]
  )]): String =
    layout(
      "Assets".some,
      div(
        cls := "mt-5",
        a(
          cls  := "btn btn-primary float-end",
          href := "/assets/new",
          "New Asset"
        ),
        table(
          cls := "table table-zebra",
          thead(
            tr(
              th("Id"),
              th("Title"),
              th("")
            )
          ),
          tbody(
            assetsViewEntries.map: (asset, _) =>
              val rowId = s"asset-${asset.id}"
              tr(
                id := rowId,
                th(asset.id.value),
                td(asset.title.value),
                td(
                  div(
                    cls := "d-flex space-x-2",
                    a(
                      cls  := "text-light",
                      href := s"/assets/edit/${asset.id}",
                      i(cls := "fa-solid fa-pen-to-square")
                    ),
                    a(
                      cls                := "text-light",
                      style              := "cursor:pointer",
                      attr("hx-delete")  := s"/assets/${asset.id}",
                      attr("hx-trigger") := "click",
                      attr("hx-target")  := s"#${rowId}",
                      attr("hx-swap")    := "outerHTML swap:1s",
                      i(cls := "fa-solid fa-trash")
                    )
                  )
                )
              )
          )
        )
      )
    )

  def renderForm(asset: Option[ExistingAsset]): String =
    val titleId = "title"
    val (hxMethod, url) = asset
      .map(asset => (attr("hx-put"), s"/assets/${asset.id}"))
      .getOrElse((attr("hx-post"), "/assets"))
    layout(
      asset.map(_.title.value).getOrElse("New Asset").some,
        div(
          cls := "mt-5 flex flex-col justify-center w-1/2 mx-auto",
          form(
            hxMethod       := url,
            attr("hx-ext") := "json-enc",
            div(
              cls := "mb-3",
              label(
                `for` := titleId,
                cls   := "input input-bordered flex items-center gap-2 w-full",
                input(
                  cls   := "grow",
                  id    := titleId,
                  name  := "title",
                  value := asset.map(_.title.value).getOrElse("")
                ),
                "Title"
              )
            ),
            button(
              `type` := "submit",
              cls    := "btn btn-primary w-full",
              "Submit"
            )
          ),
          asset
            .map: asset =>
              a(
                href := s"/asset-scraping/assets/${asset.id}/configs",
                cls  := "btn btn-secondary",
                "Scraping configs"
              )
            .getOrElse(div())
        )
    )

  def renderReleases(releases: List[Releases]): String =
    val accordionItems = releases.map: (dateUploaded, results) =>
      div(
        cls := "collapse bg-base-200 my-2",
        input(
          `type` := "radio",
          name   := "entry"
        ),
        div(
          cls := "collapse-title text-xl font-medium",
          dateUploaded.value.toString
        ),
        div(
          cls := "collapse-content",
          div(
            results
              .flatMap: (asset, entry) =>
                val headerClasses =
                  if entry.wasSeen then "" else "font-bold"
                Seq[Frag](
                  div(cls := "divider"),
                  a(
                    cls  := "justify-start w-full",
                    href := s"${entry.uri}",
                    h5(cls := headerClasses, asset.title.value),
                    p(s"Ch. ${entry.no.value}")
                  )
                )
              .tail
          )
        )
      )
    layout(
      "Releases".some,
      div(
        cls := "mt-5 collapse",
        accordionItems match
          /*
           * I don't get why, but it looks like daisyUI's second accordion item gets eaten.
           * Thus the additional div is added,
           * to make the accordion eat garbage instead of something crucial
           */
          case head :: tail => head :: div() :: tail
          case other        => other
      )
    )
