package assetImporting

import http.View.{NavBarItem, layout}
import scalatags.Text.TypedTag
import scalatags.Text.all.*

class AssetImportingView(navBarItems: List[NavBarItem]):
  import AssetImportingView.*

  def renderForm: TypedTag[String] = layout(
    Some("Import asset"),
    formPartial,
    navBarItems
  )

object AssetImportingView:
  private val formPartial =
    div(
      cls := "flex flex-col items-center",
      h3(cls := "mt-5 font-semibold", "Import from mangadex"),
      form(
        cls             := "w-full max-w-2xl",
        attr("hx-post") := "/asset-importing/mangadex",
        attr("hx-ext")  := "json-enc",
        input(
          id          := "uri",
          name        := "uri",
          `type`      := "text",
          placeholder := "https://mangadex.org/title/uu1d-l00k1ng-5tr1ng",
          cls         := "input w-full"
        ),
        button(
          cls := "btn btn-primary w-full mt-3",
          "Import"
        )
      )
    )
