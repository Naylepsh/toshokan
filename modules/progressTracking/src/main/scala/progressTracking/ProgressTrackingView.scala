package progressTracking

import cats.syntax.all.*
import http.View.{NavBarItem, layout}
import io.circe.syntax.*
import library.domain.*
import progressTracking.domain.ExistingMalMangaMapping
import progressTracking.mal.MyAnimeListClient
import scalatags.Text.TypedTag
import scalatags.Text.all.*

import domain.Manga
import schemas.NewMalMangaMappingDTO
import viewComponents.Pagination

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

  def renderMangaMalMapping(
      asset: ExistingAsset,
      mapping: ExistingMalMangaMapping
  ): TypedTag[String] =
    layout(
      asset.title.show.some,
      ProgressTrackingView.mangaMalMapping(asset, mapping),
      navbarItems
    )

  def mangaMatchesPartial(
      assetId: AssetId,
      matches: List[Manga]
  ): TypedTag[String] =
    div(
      cls := "mx-auto max-w-96",
      table(
        cls := "table table-zebra",
        thead(
          tr(
            th("External Id"),
            th("Title"),
            th("Url"),
            th("")
          )
        ),
        tbody(
          matches.map: manga =>
            tr(
              th(manga.id.show),
              td(manga.title.show),
              td(MyAnimeListClient.makeUrl(manga.id)),
              td(
                button(
                  attr("hx-post") := "/progress-tracking/mal/manga-mapping",
                  attr(
                    "hx-vals"
                  ) := NewMalMangaMappingDTO(assetId, manga.id).asJson.toString,
                  attr("hx-ext") := "json-enc",
                  "Commit"
                )
              )
            ),
        )
      )
    )

  def renderReleases(
      releases: List[Releases],
      pagination: Pagination
  ): TypedTag[String] =
    layout(
      "Releases".some,
      div(
        id  := "releases",
        cls := "mt-5",
        if releases.isEmpty then ProgressTrackingView.noReleasesPartial
        else releasesPartial(releases, pagination)
      ),
      navbarItems
    )

  def releasesPartial(
      releases: List[Releases],
      pagination: Pagination
  ): TypedTag[String] =
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
            ("btn-active", ProgressTrackingView.paginationButtonModifiers(page))
          else if page == Pagination.skipped then ("btn-disabled", Nil)
          else ("", ProgressTrackingView.paginationButtonModifiers(page))
        button(cls := s"join-item btn $className", modifiers, page)
    )

    div(
      releaseElems,
      div(
        cls := "flex",
        paginationElem
      )
    )

  def entryPartial(
      asset: ExistingAsset,
      entry: ExistingAssetEntry
  ): TypedTag[String] =
    val linkToEntry = a(
      href := s"${entry.uri}",
      p(s"[${entry.no.show}] ${entry.title.show}")
    )
    val (headerClass, icon, newState) =
      if entry.wasSeen
      then ("", i(cls := "fa-solid fa-xmark"), false)
      else ("font-bold", i(cls := "fa-solid fa-check"), true)
    val markingAction = button(
      attr(
        "hx-put"
      ) := s"/progress-tracking/partials/releases/${asset.id}/${entry.id}",
      attr("hx-vals")   := s"{\"wasEntrySeen\": ${newState}}",
      attr("hx-ext")    := "json-enc",
      attr("hx-target") := "closest .entry",
      icon
    )
    div(
      cls := "entry justify-start w-full",
      h5(cls  := headerClass, asset.title.value),
      div(cls := "flex gap-2 items-center", markingAction, linkToEntry)
    )

object ProgressTrackingView:
  private def searchMangaByTerm(mangaId: AssetId, mangaTitle: AssetTitle) =
    div(
      cls := "container",
      h2(
        cls := "font-semibold text-center mt-3",
        s"""Search for corresponding MAL entry for "${mangaTitle}""""
      ),
      form(
        cls := "mt-3",
        attr(
          "hx-get"
        ) := s"/progress-tracking/mal/manga-mapping/${mangaId.value}/search",
        attr("hx-swap") := "outerHTML",
        div(
          cls := "flex mx-auto max-w-xl",
          input(
            cls         := "input input-bordered w-full",
            name        := "term",
            placeholder := "Manga term (prefix with # to search by id)"
          ),
          button(
            `type` := "submit",
            cls    := "btn ml-2",
            "Search"
          )
        )
      )
    )

  private def paginationButtonModifiers(page: String) =
    List(
      attr("hx-get")    := s"/progress-tracking/partials/releases?page=$page",
      attr("hx-target") := "#releases"
    )

  private def noReleasesPartial =
    h2(cls := "text-center font-semibold", "No releases found")

  private def mangaMalMapping(
      asset: ExistingAsset,
      mapping: ExistingMalMangaMapping
  ) =
    val externalUrl = MyAnimeListClient.makeUrl(mapping.externalId)
    div(
      cls := "container",
      h2(
        cls := "font-semibold text-center mt-3",
        s"${asset.title.show}'s MAL Mapping"
      ),
      div(
        cls := "mx-auto max-w-2xl",
        table(
          cls := "table table-zebra",
          thead(
            tr(
              th("External Id"),
              th("External Url"),
              th("")
            )
          ),
          tbody(
            tr(
              th(mapping.externalId.show),
              td(a(href := externalUrl, externalUrl)),
              td(
                button(
                  attr(
                    "hx-delete"
                  ) := s"/progress-tracking/mal/manga-mapping/${mapping.internalId}",
                  "Delete"
                )
              )
            )
          )
        )
      )
    )
