package core

import scalatags.Text.all.*

object view:
  val template = tag("template")
  val title    = tag("title")

  def layout(
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
