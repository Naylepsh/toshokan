package assetScraping

import cats.syntax.all.*
import http.View.{ layout, template }
import library.domain.{ AssetId, ExistingAsset }
import scalatags.Text.all.*

import domain.{ ExistingAssetScrapingConfig, Site }

object AssetScrapingView:
  given Conversion[scalatags.Text.TypedTag[String], String] = _.toString

  def renderForms(
      asset: ExistingAsset,
      configs: List[ExistingAssetScrapingConfig]
  ): String =
    // TODO: Display asset id
    val configTemplateId = "config-template"
    val configGroupId    = "configs"
    layout(
      s"${asset.title}'s configs".some,
      div(
        cls := "mt-5 container flex flex-col justify-center",
        div(
          cls := "w-3/4 container mx-auto",
          div(
            cls := "grid grid-cols-8 pb-2",
            div(cls := "font-bold col-span-1", "Enabled"),
            div(cls := "font-bold col-span-1", "Id"),
            div(cls := "font-bold col-span-2", "Site"),
            div(cls := "font-bold col-span-1", "URI"),
            // Column for actions (add / update / delete / remove)
            div(cls := "col-span-2", "")
          ),
          div(
            id  := configGroupId,
            cls := "div-striped",
            configs.map(config =>
              renderConfigRow(asset.id, config.some)
            )
          )
        ),
        template(
          id := configTemplateId,
          renderConfigRow(asset.id, None)
        ),
        div(
          cls := "w-3/4 container mx-auto",
          button(
            cls     := "btn btn-primary",
            onclick := s"loadTemplate('#${configTemplateId}', '#${configGroupId}')",
            "Add new scraping config"
          )
        )
      )
    )

  def renderScrapingManagement: String =
    layout(
      "Asset Scraping".some,
      div(
        cls := "mt-5",
        a(
          cls             := "btn btn-light w-100",
          attr("hx-post") := "/asset-scraping",
          "Scrape all enabled"
        )
      )
    )

  private def renderConfigRow(
      assetId: AssetId,
      config: Option[ExistingAssetScrapingConfig]
  ) =
    var idField = span("-")
    var isEnabledModifiers =
      List(
        cls := "ml-5",
        name    := "isEnabled",
        `type`  := "checkbox",
        checked := "1",
        value   := "true"
      )
    var uriModifiers = List(name := "uri", cls := "input w-full max-w-xs")
    var hxMethod     = attr("hx-post")
    var url          = s"/asset-scraping/assets/${assetId}/configs"
    config.foreach: cfg =>
      idField = span(cfg.id.value.toString)
      isEnabledModifiers =
        (value              := cfg.isEnabled.value.toString) :: isEnabledModifiers
      uriModifiers = (value := cfg.uri.value.toString) :: uriModifiers
      hxMethod = attr("hx-put")
      url = s"/asset-scraping/assets/${assetId}/configs/${cfg.id}"

    form(
      cls               := "config-form grid grid-cols-8 mb-0 py-2",
      hxMethod          := url,
      attr("hx-ext")    := "json-enc",
      attr("hx-target") := ".config-form",
      div(
        cls := "flex",
        input(isEnabledModifiers)
      ),
      div(
        cls := "flex my-auto",
        idField
      ),
      div(
        cls := "flex col-span-2",
        select(
          name := "site",
          cls  := "select",
          Site.values.map: site =>
            var modifiers = (value := site.toString) :: Nil
            config.map(_.site).foreach:
              case s if s == site =>
                modifiers = (selected := "") :: modifiers
              case _ =>
            option(modifiers, site.toString)
        )
      ),
      div(
        cls := "flex col-span-2",
        input(uriModifiers)
      ),
      div(
        cls := "flex space-x-2",
        button(
          `type` := "submit",
          cls    := "btn",
          i(cls := "fa-solid fa-floppy-disk")
        ),
        // If config exists, this should delete it from db AND table rows
        // If config does not exist, this should remove it from table rows
        button(
          `type` := "button",
          cls    := "btn",
          i(cls := "fa-solid fa-trash")
        )
      )
    )
