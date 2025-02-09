package scraper.sites.hitomi

import java.net.URI
import java.time.LocalDate

import scala.annotation.tailrec

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Sync
import cats.syntax.all.*
import com.microsoft.playwright.{Browser, Page}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.*
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.model.Element
import scraper.domain.*

class HitomiScraper[F[_]: MonadCancelThrow: Sync](
    browser: Browser,
    timeout: Short
) extends SiteScraper[F]:

  import scraper.util.playwright.*
  import HitomiScraper.*

  override def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    browser.makePage
      .use: page =>
        scribe.cats[F].debug(s"Scraping $uri")
          *> page.navigateSafe(uri.toString)
          *> waitForContentToLoad(page).flatMap:
            case Left(error) => error.asLeft.pure
            case Right(_)    => parse(page).pure
          <* scribe.cats[F].debug(s"Done with $uri")
      .handleError(error => ScrapeError.Other(error.getMessage).asLeft)

  private def waitForContentToLoad(page: Page) =
    page
      .waitForSelectorSafe(
        ".gallery-content > div",
        new Page.WaitForSelectorOptions().setTimeout(timeout)
      )
      .map:
        case Left(_) => ScrapeError.Other("Content did not load in time").asLeft
        case Right(_) => ().asRight

  private def parse(page: Page): Either[ScrapeError, List[EntryFound]] =
    val html = JsoupBrowser().parseString(page.content())
    HitomiScraper
      .parse(html >> elementList(".gallery-content > div"))
      .flatMap:
        case Nil => ScrapeError.NoEntriesFound.asLeft
        case entries =>
          entries
            .foldLeft(List.empty[EntryFound]):
              case (acc, (entry, language)) =>
                if allowedLanguages.contains(language) then entry :: acc
                else acc
            .asRight

object HitomiScraper:
  private val allowedLanguages = List("english", "japanese", "chinese", "N/A")

  private val uriPattern = "^/(.*)/.*-([0-9]+).html$".r

  private def makeUri(href: String): URI =
    href match
      case uriPattern(entryType, id) =>
        new URI(s"https://hitomi.la/$entryType/$id")
      case _ => new URI("https", "hitomi.la", href, null)

  private def parse(
      entries: List[Element]
  ): Either[ScrapeError, List[(EntryFound, String)]] =
    parse(entries, List.empty)

  @tailrec
  private def parse(
      entries: List[Element],
      acc: List[(EntryFound, String)]
  ): Either[ScrapeError, List[(EntryFound, String)]] =
    entries match
      case Nil => acc.asRight
      case entry :: tail =>
        parseLanguage(entry) match
          case Left(error) => error.asLeft
          case Right(language) =>
            parse(
              tail,
              parse(entry).map(e => (e -> language) :: acc).getOrElse(acc)
            )

  private def parse(entry: Element): Option[EntryFound] =
    val titleElem = entry >> element(".lillie > a")
    val title     = EntryTitle(titleElem.text)
    val no        = EntryNo("")
    val href      = titleElem >> attr("href")
    val uri       = EntryUri(makeUri(href))
    parseDate(entry >> element("p.date") >> attr("data-posted")).map: date =>
      val dateUploaded = DateUploaded(date)
      EntryFound(title, no, uri, dateUploaded)

  private val languageInHrefRegex = ".*index-(.+).html".r
  private def parseLanguage(entry: Element): Either[ScrapeError, String] =
    (entry >?> element("table tr:nth-of-type(3) a[href]"))
      // cannot use `>>` because cats' impl takes precedence
      .extract(attr("href"))
      .map:
        case languageInHrefRegex(language) => language.asRight
        case other =>
          ScrapeError
            .Other(s"Could not extract language from href: $other")
            .asLeft
      .getOrElse("N/A".asRight)

  private def parseDate(rawDate: String): Option[LocalDate] =
    // It seems like there are entries with data-posted equal to an empty string
    Either.catchNonFatal(LocalDate.parse(rawDate.split(" ").head)).toOption
