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

  def renderForm(
      asset: Option[ExistingAsset],
      categories: List[ExistingCategory]
  ) =
    val titleId = "title"
    val (hxMethod, url) = asset
      .map(asset => (attr("hx-put"), s"/assets/${asset.id}"))
      .getOrElse((attr("hx-post"), "/assets"))
    layout(
      asset.map(_.title.value).getOrElse("New Asset").some,
      div(
        cls := "mt-5 flex flex-col justify-center w-1/2 mx-auto",
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
        ),
        asset
          .map: asset =>
            a(
              href := s"/asset-scraping/assets/${asset.id}/configs",
              cls  := "btn btn-secondary mt-3",
              "Scraping configs"
            )
          .getOrElse(div()),
        asset
          .map: asset =>
            a(
              href := s"/progress-tracking/mal/manga-mapping/${asset.id}",
              cls  := "btn btn-secondary mt-3",
              "MyAnimeList mapping"
            )
          .getOrElse(div())
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
