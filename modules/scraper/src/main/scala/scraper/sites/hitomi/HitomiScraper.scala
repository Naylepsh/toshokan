package scraper.sites.hitomi

import java.net.URI
import java.time.LocalDate

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Sync
import cats.syntax.all.*
import com.microsoft.playwright.{Browser, Page}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.dsl.DSL.*
import scraper.domain.*

class HitomiScraper[F[_]: MonadCancelThrow: Sync](
    browser: Browser,
    timeout: Short
) extends SiteScraper[F]:

  import scraper.util.playwright.*
  import HitomiScraper.*

  override def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    browser.makePage.use: page =>
      val preparePage =
        page.navigateSafe(uri.toString) *> waitForContentToLoad(page)
      preparePage.map(_ => parse(page))

  private def waitForContentToLoad(page: Page) =
    page.waitForSelectorSafe(
      ".gallery-content > div",
      new Page.WaitForSelectorOptions().setTimeout(timeout)
    )

  private def parse(page: Page) =
    val parser  = JsoupBrowser()
    val html    = parser.parseString(page.content())
    val entries = html >> elementList(".gallery-content > div")
    val results = entries
      .map: entry =>
        val titleElem = entry >> element(".lillie > a")
        val no        = EntryNo(titleElem.text)
        val href      = titleElem >> attr("href")
        val uri       = EntryUri(makeUri(href))
        val dateUploaded = DateUploaded(
          parseDate(entry >> element("p.date") >> attr("data-posted"))
        )
        EntryFound(no, uri, dateUploaded)
    results match
      case Nil     => ScrapeError.NoEntriesFound.asLeft
      case entries => entries.asRight

object HitomiScraper:
  def parseDate(rawDate: String) = LocalDate.parse(rawDate.split(" ").head)

  def makeUri(href: String) = new URI("https", "hitomi.la", href, null)
