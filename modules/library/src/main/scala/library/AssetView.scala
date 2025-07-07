package library

import cats.syntax.all.*
import http.View.{NavBarItem, layout}
import library.domain.*
import scalatags.Text.TypedTag
import scalatags.Text.all.*

import category.domain.*

class AssetView(navBarItems: List[NavBarItem]):
  import AssetView.*

  def renderAssets(
      assetsViewEntries: List[
        (
            ExistingAsset,
            List[ExistingAssetEntry]
        )
      ]
  ): TypedTag[String] =
    layout(
      "Assets".some,
      div(
        cls := "mt-5",
        a(
          cls  := "btn btn-primary float-end",
          href := "/asset-importing",
          "Import asset"
        ),
        a(
          cls  := "btn btn-primary float-end mr-2",
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
                      href := s"/assets/${asset.id}",
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
      ),
      navBarItems
    )

  def renderAsset(
      asset: ExistingAsset,
      entries: List[ExistingAssetEntry],
      categories: List[ExistingCategory]
  ): TypedTag[String] =
    layout(
      asset.title.show.some,
      assetDetailesPartial(asset, entries, categories),
      navBarItems
    )

  def renderForm(categories: List[ExistingCategory]): TypedTag[String] =
    layout(
      "New Asset".some,
      div(
        cls := "mt-5 flex flex-col justify-center w-1/2 mx-auto",
        formPartial(None, categories)
      ),
      navBarItems
    )

object AssetView:
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
              if head.id.some == selectedCategory then
                selected := selectedCategory.get.show
              else (),
              head.name.show
            ),
            tail.map: category =>
              option(
                value := category.id.value,
                if category.id.some == selectedCategory then
                  selected := selectedCategory.get.show
                else (),
                category.name.show
              )
          )
        )

  private def assetDetailesPartial(
      asset: ExistingAsset,
      entries: List[ExistingAssetEntry],
      categories: List[ExistingCategory]
  ): TypedTag[String] =
    div(
      cls := "mt-5 flex flex-col justify-center w-1/2 mx-auto",
      formPartial(asset.some, categories),
      a(
        href := s"/asset-scraping/assets/${asset.id}/configs",
        cls  := "btn btn-secondary mt-3",
        "Scraping configs"
      ),
      a(
        href := s"/asset-mapping/${asset.id}",
        cls  := "btn btn-secondary mt-3",
        "Mapping to external services"
      ),
      div(
        cls := "mt-3",
        div(
          cls := "flex justify-end",
          a(
            cls             := "btn btn-primary btn-wide mt-3 mr-3",
            attr("hx-post") := s"/asset-scraping/asset/${asset.id}",
            attr("hx-swap") := "none",
            attr("hx-on::after-request") := "window.location.reload()",
            "Get new releases"
          ),
          button(
            cls := "btn btn-primary btn-wide mt-3",
            attr(
              "hx-patch"
            ) := s"/progress-tracking/partials/releases/${asset.id}",
            "Binge"
          )
        ),
        table(
          cls := "table table-zebra",
          thead(
            tr(
              th("Id"),
              th("No"),
              th("Title"),
              th("Url"),
              th("Actions")
            )
          ),
          tbody(
            entries.map: entry =>
              tr(
                th(entry.id.show),
                td(entry.no.show),
                td(entry.title.show),
                td(a(href := entry.uri.show, entry.uri.show)),
                td(
                  div(
                    button(
                      attr("hx-post") := s"/asset-downloading/${entry.id}",
                      attr("hx-swap") := "none",
                      attr("_", raw = true) := s"""
                        on click 
                          add .hidden to <.download/> in me
                          then remove .hidden from <.spinner/> in me
                        end
                        on htmx:afterRequest
                          add .hidden to <.spinner/> in me
                          then remove .hidden from <.download-success/> in me
                        end
                      """,
                      i(
                        cls := "fa-solid fa-file-import download"
                      ),
                      div(cls := "spinner hidden"),
                      i(
                        cls := "fa-solid fa-check download-success hidden"
                      )
                    )
                  )
                )
              )
          )
        )
      )
    )

  private def formPartial(
      asset: Option[ExistingAsset],
      categories: List[ExistingCategory]
  ): TypedTag[String] =
    val titleId = "title"
    val (hxMethod, url) = asset
      .map(asset => (attr("hx-put"), s"/assets/${asset.id}"))
      .getOrElse((attr("hx-post"), "/assets"))
    form(
      cls            := "mb-0",
      hxMethod       := url,
      attr("hx-ext") := "json-enc",
      div(
        label(
          `for` := titleId,
          cls   := "form-control w-full",
          div(cls := "label", span(cls := "label-text", "Title")),
          input(
            cls   := "input input-bordered w-full",
            id    := titleId,
            name  := "title",
            value := asset.map(_.title.value).getOrElse("")
          )
        )
      ),
      div(
        cls := "mt-3",
        categorySelectionPartial(categories, asset.flatMap(_.categoryId))
      ),
      button(
        `type` := "submit",
        cls    := "btn btn-primary w-full mt-3",
        "Submit"
      )
    )
