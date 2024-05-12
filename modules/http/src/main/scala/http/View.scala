package http

import scalatags.Text.all.*

object View:
  val template = tag("template")
  val title    = tag("title")

  case class NavBarItem(label: String, ref: String)

  def layout(
      subTitle: Option[String],
      bodyContent: scalatags.Text.Modifier,
      navBarItems: List[NavBarItem] = Nil
  ) =
    val titleContent = subTitle match
      case Some(sub) => s"$sub | Toshokan"
      case None      => "Toshokan"
    html(
      attr("data-theme") := "cupcake",
      head(
        title(titleContent),
        meta(
          httpEquiv := "Content-Type",
          content   := "text/html; charset=utf-8"
        ),
        link(
          href := "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css",
          rel := "stylesheet"
        ),
        link(
          href := "https://cdn.jsdelivr.net/npm/daisyui@4.9.0/dist/full.min.css",
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
        navBar(navBarItems),
        div(cls := "container mx-auto mb-4", bodyContent)
      )
    )

  /** This navbar does not support small resolutions
    */
  def navBar(items: List[NavBarItem]) =
    div(
      cls := "navbar bg-accent sticky top-0 z-10",
      div(
        cls := "navbar-start",
        a(cls := "btn btn-ghost text-xl", "Toshokan"),
        ul(
          cls := "menu menu-horizontal",
          items.map: item =>
            li(
              a(href := item.ref, item.label)
            )
        )
      )
    )
