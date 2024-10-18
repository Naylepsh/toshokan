package progressTracking
package assetMapping

import cats.syntax.all.*
import http.View.{NavBarItem, layout}
import io.circe.syntax.*
import library.domain.*
import scalatags.Text.TypedTag
import scalatags.Text.all.*

import domain.{Manga, ExistingMalMangaMapping}
import mal.MyAnimeListClient
import schemas.NewMalMangaMappingDTO

class AssetMappingView(navBarItems: List[NavBarItem]):
  def renderMangaSearch(
      mangaTitle: AssetTitle,
      mangaId: AssetId
  ): TypedTag[String] =
    layout(
      mangaTitle.show.some,
      AssetMappingView.searchMangaByTerm(mangaId, mangaTitle),
      navBarItems
    )

  def renderMangaMalMapping(
      asset: ExistingAsset,
      mapping: ExistingMalMangaMapping
  ): TypedTag[String] =
    layout(
      asset.title.show.some,
      AssetMappingView.mangaMalMapping(asset, mapping),
      navBarItems
    )

  def mangaMatchesPartial(
      assetId: AssetId,
      matches: List[Manga]
  ): TypedTag[String] =
    div(
      cls := "mx-auto max-w-2xl",
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
            val externalUrl = MyAnimeListClient.makeUrl(manga.id)
            tr(
              th(manga.id.show),
              td(manga.title.show),
              td(a(href := externalUrl, externalUrl)),
              td(
                button(
                  attr("hx-post") := "/asset-mapping",
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

object AssetMappingView:
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
        )               := s"/asset-mapping/${mangaId.value}/search",
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
                  ) := s"/asset-mapping/${mapping.internalId}",
                  "Delete"
                )
              )
            )
          )
        )
      )
    )
