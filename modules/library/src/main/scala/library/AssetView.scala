package library

import cats.syntax.all.*
import library.domain.*
import org.http4s.dsl.Http4sDsl
import org.http4s.{ EntityDecoder, * }
import scalatags.Text.all.*

trait AssetView[F[_], A](using EntityDecoder[F, A]):
  val mediaType: MediaType
  def renderAssets(assetsViewEntries: List[(
      ExistingAsset,
      List[ExistingAssetEntry]
  )]): A
  def renderForm(asset: Option[ExistingAsset]): A
  def renderReleases(releases: List[Releases]): A

object AssetView:
  given Conversion[scalatags.Text.TypedTag[String], String] = _.toString

  def makeHtmlView[F[_]](using EntityDecoder[F, String]): AssetView[F, String] =
    new:
      val mediaType: MediaType = MediaType.text.html

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
        val configTemplateId = "config-template"
        val configGroupId    = "configs"
        val titleId          = "title"
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
                      List.empty[ExistingAssetScrapingConfig].map(config =>
                        renderConfigRow(asset.id, config.some)
                      )
                    )
                  ),
                  template(
                    id := configTemplateId,
                    renderConfigRow(asset.id, None)
                  ),
                  button(
                    cls     := "btn btn-light",
                    onclick := s"loadTemplate('#${configTemplateId}', '#${configGroupId}')",
                    "Add new scraping config"
                  )
                )
              .getOrElse(div())
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

      def renderReleases(releases: List[Releases]): String =
        layout(
          "Releases".some,
          ???
        )

      private def renderAccordion(assetsViewEntries: List[(
          ExistingAsset,
          List[ExistingAssetEntry]
      )]): String =
        /**
         * TODO: This will be used for entries view ig?
         */
        val accordionId = "asset-list"
        layout(
          "Assets".some,
          div(
            id  := accordionId,
            cls := "accordion mt-5",
            assetsViewEntries.map: (asset, _) =>
              val accordionItemId = s"asset${asset.id.value}"
              div(
                cls := "accordion-item",
                h2(
                  cls := "accordion-header",
                  button(
                    cls                    := "accordion-button",
                    `type`                 := "button",
                    attr("data-bs-toggle") := "collapse",
                    attr("data-bs-target") := s"#${accordionItemId}",
                    asset.title.value
                  )
                ),
                div(
                  id                     := accordionItemId,
                  cls                    := "accordion-collapse collapse",
                  attr("data-bs-parent") := s"#${accordionId}",
                  div(cls := "accordion-body", "Foo, bar, baz")
                )
              )
          )
        ).toString

  private val template = tag("template")
  private val title    = tag("title")

  private def layout(
      subTitle: Option[String],
      bodyContent: scalatags.Text.Modifier*
  ) =
    val titleContent = subTitle match
      case Some(sub) => s"$sub | Toshokan"
      case None      => "Toshokan"
    html(
      attr("data-bs-theme") := "dark",
      head(
        title(titleContent),
        link(
          href := "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css",
          rel  := "stylesheet"
        ),
        link(
          href := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css",
          rel  := "stylesheet"
        ),
        link(
          href := "/public/css/index.css",
          rel  := "stylesheet"
        ),
        script(
          src := "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"
        ),
        script(src    := "https://unpkg.com/htmx.org@1.9.4"),
        script(src    := "https://unpkg.com/htmx.org/dist/ext/json-enc.js"),
        script(src    := "https://unpkg.com/hyperscript.org@0.9.11"),
        script(`type` := "text/javascript", src := "/public/js/index.js")
      ),
      body(
        cls := "container",
        bodyContent
      )
    )
