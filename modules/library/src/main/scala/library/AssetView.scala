package library

import cats.syntax.all.*
import core.given
import http.View.{NavBarItem, dialog, layout}
import library.domain.*
import neotype.*
import neotype.interop.cats.given
import scalatags.Text.TypedTag
import scalatags.Text.all.*

import category.domain.*

class AssetView(navBarItems: List[NavBarItem]):
  import AssetView.*

  def renderAssets(
      assetsViewEntries: List[
        (
            ExistingAsset,
            Option[CategoryName],
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
              th("Category"),
              th("")
            )
          ),
          tbody(
            assetsViewEntries.map: (asset, categoryName, _) =>
              val rowId = s"asset-${asset.id}"
              tr(
                id := rowId,
                th(asset.id.unwrap.show),
                td(asset.title),
                td(categoryName.getOrElse("-")),
                td(
                  div(
                    cls := "flex gap-2",
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

  def renderStaleAssets(
      assets: List[StaleAsset]
  ): TypedTag[String] =
    layout(
      "Stale Assets".some,
      div(
        cls := "mt-5",
        div(
          cls := "mb-6",
          h2(
            cls := "text-2xl font-bolt text-center mb-2",
            "Assets with no recent releases"
          )
        ),
        table(
          cls := "table table-zebra",
          thead(
            tr(
              th("Title"),
              th("Last Release"),
              th("Days Ago"),
              th("")
            )
          ),
          tbody(
            assets.map: asset =>
              tr(
                td(asset.asset.title),
                td(asset.lastRelease.toString),
                td(asset.daysSinceLastRelease.toString),
                td(
                  a(
                    cls  := "text-light",
                    href := s"/assets/${asset.asset.id}",
                    i(cls := "fa-solid fa-eye")
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
      assetDetailesPartial(
        asset,
        entries.sortBy(_.no)(using Ordering[EntryNo].reverse),
        categories
      ),
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
              value := head.id.unwrap,
              if head.id.some == selectedCategory then
                selected := selectedCategory.get.show
              else (),
              head.name.show
            ),
            tail.map: category =>
              option(
                value := category.id.unwrap,
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
          cls := "flex justify-between gap-3",
          a(
            cls             := "btn btn-primary flex-1 mt-3",
            attr("hx-post") := s"/asset-scraping/asset/${asset.id}",
            attr("hx-swap") := "none",
            attr("hx-on::after-request") := "window.location.reload()",
            "Get new releases"
          ),
          button(
            cls               := "btn btn-secondary flex-1 mt-3",
            attr("hx-post")   := s"/asset-downloading/bulk/${asset.id}",
            attr("hx-swap")   := "innerHTML",
            attr("hx-target") := "#bulk-download-modal-content",
            attr("hx-on::after-request") := "bulk_download_modal.showModal()",
            "Bulk Download"
          ),
          button(
            cls := "btn btn-primary flex-1 mt-3",
            attr(
              "hx-patch"
            ) := s"/progress-tracking/partials/releases/${asset.id}",
            "Binge"
          )
        ),
        dialog(
          id  := "bulk_download_modal",
          cls := "modal",
          div(
            id  := "bulk-download-modal-content",
            cls := "modal-box w-11/12 max-w-5xl"
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
            value := asset.map(_.title).getOrElse("")
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
