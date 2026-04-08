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

private type Language = Language.Type
private object Language extends neotype.Subtype[String]

class HitomiScraper[F[_]: MonadCancelThrow: Sync](
    browser: Browser,
    timeout: Short
) extends SiteScraperOfAuthor[F]:

  import scraper.util.playwright.*
  import HitomiScraper.*

  override def scrapeForAssets(
      uri: AuthorScrapingUri
  ): F[Either[ScrapeError, List[AssetFound]]] =
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

  private def parse(page: Page): Either[ScrapeError, List[AssetFound]] =
    val html = JsoupBrowser().parseString(page.content())
    HitomiScraper
      .parse(html >> elementList(".gallery-content > div"))
      .map: assets =>
        assets
          .foldLeft(List.empty[AssetFound]):
            case (acc, (asset, language)) =>
              if allowedLanguages.contains(language) then asset :: acc
              else acc

object HitomiScraper:
  private val allowedLanguages =
    Language.applyAll("english", "japanese", "chinese", "N/A")

  private val uriPattern = "^/(.*)/.*-([0-9]+).html$".r

  private def makeUri(href: String): URI =
    href match
      case uriPattern(entryType, id) =>
        new URI(s"https://hitomi.la/$entryType/$id")
      case _ => new URI("https", "hitomi.la", href, null)

  private def parse(
      entries: List[Element]
  ): Either[ScrapeError, List[(AssetFound, Language)]] =
    parse(entries, List.empty)

  @tailrec
  private def parse(
      entries: List[Element],
      acc: List[(AssetFound, Language)]
  ): Either[ScrapeError, List[(AssetFound, Language)]] =
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

  private def parse(entry: Element): Either[ScrapeError, AssetFound] =
    val titleElem = entry >> element(".lillie > a")
    val title     = EntryTitle(titleElem.text)
    val no        = EntryNo("")
    val href      = titleElem >> attr("href")
    val uri       = EntryUri(makeUri(href))
    for
      artists <- (entry >> elements(".artist-list a[href]") >> texts).toList
        .traverse(Author.make)
        .map(_.toSet)
        .leftMap(error => ScrapeError.Other(error))
      entry <- Either.fromOption(
        parseDate(entry >> element("p.date") >> attr("data-posted")).map {
          date =>
            val dateUploaded = DateUploaded(date)
            EntryFound(title, no, uri, dateUploaded)
        },
        ScrapeError.Other(s"No date uploaded for ${title}")
      )
    yield AssetFound.ofSingleEntry(entry, artists)

  private val languageInHrefRegex = ".*index-(.+).html".r
  private def parseLanguage(entry: Element): Either[ScrapeError, Language] =
    (entry >?> element("table tr:nth-of-type(3) a[href]"))
      // cannot use `>>` because cats' impl takes precedence
      .extract(attr("href"))
      .map:
        case languageInHrefRegex(language) =>
          Language.make(language).leftMap(ScrapeError.Other(_))
        case other =>
          ScrapeError
            .Other(s"Could not extract language from href: $other")
            .asLeft
      .getOrElse(Language("N/A").asRight)

  private def parseDate(rawDate: String): Option[LocalDate] =
    // It seems like there are entries with data-posted equal to an empty string
    Either.catchNonFatal(LocalDate.parse(rawDate.split(" ").head)).toOption
