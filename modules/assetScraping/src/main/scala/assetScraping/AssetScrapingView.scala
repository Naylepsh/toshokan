package assetScraping

import cats.syntax.all.*
import core.view.{ layout, template }
import library.domain.AssetId
import org.http4s.{ EntityDecoder, * }
import scalatags.Text.all.*

import domain.{ ExistingAssetScrapingConfig, Site }

trait AssetScrapingView[F[_], A]:
  val mediaType: MediaType
  def renderForms(
      assetId: AssetId,
      configs: List[ExistingAssetScrapingConfig]
  ): A

object AssetScrapingView:
  def makeHtmlView[F[_]](using
  EntityDecoder[F, String]): AssetScrapingView[F, String] = new:
    given Conversion[scalatags.Text.TypedTag[String], String] = _.toString

    val mediaType: MediaType = MediaType.text.html

    def renderForms(
        assetId: AssetId,
        configs: List[ExistingAssetScrapingConfig]
    ): String =
      // TODO: Display asset id
      val configTemplateId = "config-template"
      val configGroupId    = "configs"
      val titleId          = "title"
      layout(
        s"${assetId} configs".some,
        div(
          cls := "mt-5",
          // TODO: Add the new config handling
          div(
            div(
              cls := "container",
              div(
                cls := "row pb-2",
                div(cls := "col-1 fw-bold", "Enabled"),
                div(cls := "col-1 fw-bold", "Id"),
                div(cls := "col-2 fw-bold", "Site"),
                div(cls := "col fw-bold", "URI"),
                // Column for actions (add / update / delete / remove)
                div(cls := "col-1", "")
              ),
              div(
                id  := configGroupId,
                cls := "div-striped",
                // TODO: Use actual asset's configs
                configs.map(config =>
                  renderConfigRow(assetId, config.some)
                )
              )
            ),
            template(
              id := configTemplateId,
              renderConfigRow(assetId, None)
            ),
            button(
              cls     := "btn btn-light",
              onclick := s"loadTemplate('#${configTemplateId}', '#${configGroupId}')",
              "Add new scraping config"
            )
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
          name    := "isEnabled",
          `type`  := "checkbox",
          checked := "1",
          value   := "true"
        )
      var uriModifiers = List(name := "uri", cls := "w-100")
      var hxMethod     = attr("hx-post")
      var url          = s"/assets/${assetId}/scraping/configs"
      config.foreach: cfg =>
        idField = input(name := "id", value := cfg.id.value.toString)
        isEnabledModifiers =
          (value              := cfg.isEnabled.value.toString) :: isEnabledModifiers
        uriModifiers = (value := cfg.uri.value.toString) :: uriModifiers
        hxMethod = attr("hx-put")
        url = s"/assets/${assetId}/scraping/configs/${cfg.id}"

      form(
        cls               := "config-form row mb-0 py-2",
        hxMethod          := url,
        attr("hx-ext")    := "json-enc",
        attr("hx-target") := ".config-form",
        div(
          cls := "col-1 d-flex",
          input(isEnabledModifiers)
        ),
        div(
          cls := "col-1",
          idField
        ),
        div(
          cls := "col-2",
          select(
            name := "site",
            cls  := "form-select",
            Site.values.map: site =>
              option(value := site.toString, site.toString)
          )
        ),
        div(
          cls := "col d-flex",
          input(uriModifiers)
        ),
        div(
          cls := "col-2 d-flex",
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