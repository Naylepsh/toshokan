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
          cls  := "btn btn-light float-end",
          href := "/assets/new",
          "New Asset"
        ),
        table(
          cls := "table table-striped",
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
                    cls := "d-flex gap-2",
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
        cls := "mt-5",
        form(
          hxMethod       := url,
          attr("hx-ext") := "json-enc",
          div(
            cls := "mb-3",
            label(`for` := titleId, cls := "form-label", "Title"),
            input(
              cls   := "form-control",
              id    := titleId,
              name  := "title",
              value := asset.map(_.title.value).getOrElse("")
            )
          ),
          button(`type` := "submit", cls := "btn btn-light w-100", "Submit")
        ),
        asset
          .map: asset =>
            a(
              href := s"/asset-scraping/assets/${asset.id}/configs",
              cls  := "btn btn-light",
              "Scraping configs"
            )
          .getOrElse(div())
      )
    )

  def renderReleases(releases: List[Releases]): String =
    val accordionId = "releases"
    layout(
      "Releases".some,
      div(
        id  := accordionId,
        cls := "accordion mt-5",
        releases.map: (dateUploaded, results) =>
          /**
           * Can't use s"{dateUploaded.value}" as id,
           * because a selector with a leading number is not a valid CSS selector,
           * which causes issues with bootstrap
           */
          val accordionItemId = s"date-${dateUploaded.value}"
          div(
            cls := "accordion-item",
            h2(
              cls := "accordion-header",
              button(
                cls                    := "accordion-button",
                `type`                 := "button",
                attr("data-bs-toggle") := "collapse",
                attr("data-bs-target") := s"#${accordionItemId}",
                dateUploaded.value.toString
              )
            ),
            div(
              id                     := accordionItemId,
              cls                    := "accordion-collapse collapse",
              attr("data-bs-parent") := s"#${accordionId}",
              div(
                cls := "accordion-body",
                results.map: (asset, entry) =>
                  val headerClasses =
                    if entry.wasSeen then "" else "fw-bold"
                  a(
                    cls  := "btn text-start",
                    href := s"${entry.uri}",
                    h5(cls := headerClasses, asset.title.value),
                    p(s"Ch. ${entry.no.value}")
                  )
              )
            )
          )
      )
    )
