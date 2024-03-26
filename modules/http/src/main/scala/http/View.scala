package http

import scalatags.Text.all.*

object View:
  val template = tag("template")
  val title    = tag("title")

  /**
   * <link href="https://cdn.jsdelivr.net/npm/daisyui@4.9.0/dist/full.min.css" rel="stylesheet" type="text/css" />
   * <script src="https://cdn.tailwindcss.com"></script>
   */

  def layout(
      subTitle: Option[String],
      bodyContent: scalatags.Text.Modifier*
  ) =
    val titleContent = subTitle match
      case Some(sub) => s"$sub | Toshokan"
      case None      => "Toshokan"
    html(
      attr("data-theme") := "cupcake",
      head(
        title(titleContent),
        link(
          href := "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css",
          rel  := "stylesheet"
        ),
        link(
          href   := "https://cdn.jsdelivr.net/npm/daisyui@4.9.0/dist/full.min.css",
          rel    := "stylesheet",
          `type` := "text/css"
        ),
        link(
          href := "/public/css/index.css",
          rel  := "stylesheet"
        ),
        script(src    := "https://unpkg.com/htmx.org@1.9.4"),
        script(src    := "https://unpkg.com/htmx.org/dist/ext/json-enc.js"),
        script(src    := "https://unpkg.com/hyperscript.org@0.9.11"),
        script(`type` := "text/javascript", src := "/public/js/index.js"),
        script(src    := "https://cdn.tailwindcss.com")
      ),
      body(
        cls := "container mx-auto",
        bodyContent
      )
    )
