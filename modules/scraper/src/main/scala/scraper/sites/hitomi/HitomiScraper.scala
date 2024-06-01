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
import net.ruippeixotog.scalascraper.model.Element
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

  private def parse(page: Page): Either[ScrapeError, List[EntryFound]] =
    val parser  = JsoupBrowser()
    val html    = parser.parseString(page.content())
    val entries = html >> elementList(".gallery-content > div")
    val results = entries.foldLeft(List.empty[EntryFound]): (acc, entry) =>
      parseLanguage(entry) match
        case Some(language)
            if List("english", "japanese", "chinese").contains(language) =>
          HitomiScraper.parse(entry) :: acc
        case _ => acc

    results match
      case Nil     => ScrapeError.NoEntriesFound.asLeft
      case entries => entries.asRight

object HitomiScraper:
  private def parseDate(rawDate: String): LocalDate =
    LocalDate.parse(rawDate.split(" ").head)

  private def makeUri(href: String): URI =
    new URI("https", "hitomi.la", href, null)

  private def parse(entry: Element): EntryFound =
    val titleElem = entry >> element(".lillie > a")
    val no        = EntryNo(titleElem.text)
    val href      = titleElem >> attr("href")
    val uri       = EntryUri(makeUri(href))
    val dateUploaded = DateUploaded(
      parseDate(entry >> element("p.date") >> attr("data-posted"))
    )
    EntryFound(no, uri, dateUploaded)

  private val languageInHrefRegex = "index-(.+).html".r
  private def parseLanguage(entry: Element): Option[String] =
    val languageHref =
      entry >> element("table tr:nth-of-type(3) a") >> attr("href")
    languageHref match
      case languageInHrefRegex(language) => language.some
      case other =>
        scribe.error(s"Could not extract lanuage from href: $other")
        None
