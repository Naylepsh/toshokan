package library

import cats.syntax.all.*
import library.domain.*
import scalatags.Text.all.*
import http.View.layout
import http.View.NavBarItem

class AssetView(navBarItems: List[NavBarItem]):
  import AssetView.*

  def renderAssets(assetsViewEntries: List[(
      ExistingAsset,
      List[ExistingAssetEntry]
  )]) =
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
      ),
      navBarItems
    )

  def renderForm(asset: Option[ExistingAsset]) =
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
      ),
      navBarItems
    )

  def renderReleases(releases: List[Releases], pagination: Pagination) =
    layout(
      "Releases".some,
      div(
        id  := "releases",
        cls := "mt-5",
        releasesPartial(releases, pagination)
      ),
      navBarItems
    )

  def releasesPartial(releases: List[Releases], pagination: Pagination) =
    val releaseElems = releases.map: (dateUploaded, results) =>
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
                Seq[Frag](
                  div(cls := "divider"),
                  entryPartial(asset, entry)
                )
              .tail
          )
        )
      )
    val paginationElem = div(
      cls := "join mx-auto",
      pagination.pages.map: page =>
        val (className, modifiers) =
          if page == pagination.current.toString then
            ("btn-active", paginationButtonModifiers(page))
          else if page == Pagination.skipped then ("btn-disabled", Nil)
          else ("", paginationButtonModifiers(page))
        button(cls := s"join-item btn $className", modifiers, page)
    )
    div(
      releaseElems,
      div(
        cls := "flex",
        paginationElem
      )
    )

  def entryPartial(asset: ExistingAsset, entry: ExistingAssetEntry) =
    val linkToEntry = a(
      href := s"${entry.uri}",
      p(s"Ch. ${entry.no.value}")
    )
    val (headerClass, icon, newState) =
      if entry.wasSeen
      then ("", i(cls := "fa-solid fa-xmark"), false)
      else ("font-bold", i(cls := "fa-solid fa-check"), true)
    val markingAction = button(
      attr("hx-patch")  := s"/assets/${asset.id}/entries/${entry.id}",
      attr("hx-vals")   := s"{\"wasSeen\": ${newState}}",
      attr("hx-ext")    := "json-enc",
      attr("hx-target") := "closest .entry",
      icon
    )
    div(
      cls := "entry justify-start w-full",
      h5(cls  := headerClass, asset.title.value),
      div(cls := "flex gap-2 items-center", markingAction, linkToEntry)
    )

object AssetView:
  private def paginationButtonModifiers(page: String) =
    List(
      attr("hx-get")    := s"/assets/partials/entries-by-release-date?page=$page",
      attr("hx-target") := "#releases"
    )
