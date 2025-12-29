package assetScraping.configs

import cats.syntax.all.*
import http.View.{NavBarItem, layout, template}
import library.domain.{AssetId, ExistingAsset}
import neotype.*
import scalatags.Text.all.*

import domain.{ExistingAssetScrapingConfig, Site}

class AssetScrapingConfigView(navBarItems: List[NavBarItem]):
  given Conversion[scalatags.Text.TypedTag[String], String] = _.toString

  def renderForms(
      asset: ExistingAsset,
      configs: List[ExistingAssetScrapingConfig]
  ): String =
    val configTemplateId = "config-template"
    val configGroupId    = "configs"
    layout(
      s"${asset.title}'s configs".some,
      div(
        cls := "mt-5 container flex flex-col justify-center",
        div(
          cls := "w-3/4 container mx-auto",
          div(
            cls := "grid grid-cols-12 pb-2",
            div(cls := "font-bold col-span-1", "Enabled"),
            div(cls := "font-bold col-span-1", "Id"),
            div(cls := "font-bold col-span-2", "Site"),
            div(cls := "font-bold col-span-6", "URI"),
            // Column for actions (add / update / delete / remove)
            div(cls := "col-span-2", "")
          ),
          div(
            id  := configGroupId,
            cls := "div-striped",
            configs.map(config => renderConfigRow(asset.id, config.some))
          )
        ),
        template(
          id := configTemplateId,
          renderConfigRow(asset.id, None)
        ),
        div(
          cls := "w-3/4 container mx-auto mt-4",
          button(
            cls := "btn btn-primary",
            onclick := s"loadTemplate('#${configTemplateId}', '#${configGroupId}')",
            "Add new scraping config"
          )
        )
      ),
      navBarItems
    )

  def renderConfigRow(
      assetId: AssetId,
      config: Option[ExistingAssetScrapingConfig]
  ) =
    var idField = span("-")
    var isEnabledModifiers =
      List(
        cls    := "ml-5",
        name   := "isEnabled",
        `type` := "checkbox",
        value  := "true"
      )
    var uriModifiers    = List(name := "uri", cls := "input w-full mr-4")
    var hxMethod        = attr("hx-post")
    var url             = s"/asset-scraping/assets/${assetId}/configs"
    var deleteModifiers = List(`type` := "button", cls := "btn")
    config match
      case Some(cfg) =>
        idField = span(cfg.id.unwrap.toString)
        if cfg.isEnabled then
          isEnabledModifiers = (checked := "1") :: isEnabledModifiers
        uriModifiers = (value := cfg.uri.toString) :: uriModifiers
        hxMethod = attr("hx-put")
        url = s"/asset-scraping/assets/${assetId}/configs/${cfg.id}"
        deleteModifiers = (attr("hx-delete") := url)
          :: (attr("hx-target")              := "closest .config-form")
          :: deleteModifiers
      case None =>
        isEnabledModifiers = (checked := "1") :: isEnabledModifiers
        deleteModifiers =
          (onclick := "removeClosest(this, '.config-form')") :: deleteModifiers

    form(
      cls               := "config-form grid grid-cols-12 mb-0 py-2",
      hxMethod          := url,
      attr("hx-ext")    := "json-enc",
      attr("hx-target") := ".config-form",
      attr("hx-swap")   := "outerHTML",
      div(
        cls := "flex col-span-1",
        input(isEnabledModifiers)
      ),
      div(
        cls := "flex col-span-1 my-auto",
        idField
      ),
      div(
        cls := "flex col-span-2",
        select(
          name := "site",
          cls  := "select bg-transparent",
          Site.values.map: site =>
            var modifiers = (value := site.toString) :: Nil
            config
              .map(_.site)
              .foreach:
                case s if s == site =>
                  modifiers = (selected := "") :: modifiers
                case _ =>
            option(modifiers, site.toString)
        )
      ),
      div(
        cls := "flex col-span-6",
        input(uriModifiers)
      ),
      div(
        cls := "flex col-span-2 space-x-2",
        button(
          `type` := "submit",
          cls    := "btn",
          i(cls := "fa-solid fa-floppy-disk")
        ),
        // If config exists, this should delete it from db AND table rows
        // If config does not exist, this should remove it from table rows
        button(
          deleteModifiers,
          i(cls := "fa-solid fa-trash")
        )
      )
    )
