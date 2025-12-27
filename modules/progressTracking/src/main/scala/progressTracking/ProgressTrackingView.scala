package progressTracking

import cats.syntax.all.*
import http.View.{NavBarItem, layout}
import library.domain.*
import neotype.interop.cats.given
import org.typelevel.cats.time.*
import scalatags.Text.TypedTag
import scalatags.Text.all.*

import viewComponents.Pagination

class ProgressTrackingView(navbarItems: List[NavBarItem]):
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
          dateUploaded.show
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
      a(
        href := s"/assets/${asset.id}",
        h5(cls := headerClass, asset.title.show)
      ),
      div(cls := "flex gap-2 items-center", markingAction, linkToEntry)
    )

object ProgressTrackingView:
  private def paginationButtonModifiers(page: String) =
    List(
      attr("hx-get")    := s"/progress-tracking/partials/releases?page=$page",
      attr("hx-target") := "#releases"
    )

  private def noReleasesPartial =
    h2(cls := "text-center font-semibold", "No releases found")
