package scraper.sites.chainedSoldier

import java.net.URI
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale

import cats.effect.IO
import cats.syntax.all.*
import net.ruippeixotog.scalascraper.dsl.DSL.*
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.model.Document
import scraper.domain.*
import scraper.util.requests.getHtmlContent

class ChainedSoldierScraper extends SiteScraper:

  override def findEntries(
      uri: URI
  ): IO[Either[ScrapeError, List[EntryFound]]] =
    getHtmlContent(uri).map:
      case Left(error)    => ScrapeError.Other(error.toString).asLeft
      case Right(content) => ChainedSoldierScraper.parseContent(content)

object ChainedSoldierScraper:
  private val chapterNoPattern = """Chapter (\d+(?:\.\d+)?)""".r
  private val dateFormatter =
    DateTimeFormatter.ofPattern("MMMM d, yyyy", Locale.ENGLISH)

  def parseContent(
      document: Document
  ): Either[ScrapeError, List[EntryFound]] =
    val items =
      document >> elementList("#chapters-list-holder a.chapter-list-item")
    if items.isEmpty then ScrapeError.NoEntriesFound.asLeft
    else
      items.traverse: item =>
        val name = (item >> element("span.chapter-name")).text
        val date = (item >> element("span.chapter-date")).text
        val href = item.attr("href")

        val title = EntryTitle(name)
        val no = name match
          case chapterNoPattern(n) => EntryNo(n).asRight
          case _ =>
            ScrapeError
              .Other(s"Could not extract chapter no from: $name")
              .asLeft

        val uri          = EntryUri(href).leftMap(ScrapeError.Other(_))
        val dateUploaded = parseDate(date)

        (no, uri, dateUploaded).tupled.map(EntryFound(title, _, _, _))

  private def parseDate(raw: String): Either[ScrapeError, DateUploaded] =
    Either
      .catchNonFatal(LocalDate.parse(raw, dateFormatter))
      .bimap(
        e => ScrapeError.Other(s"Could not parse date '$raw': ${e.getMessage}"),
        DateUploaded(_)
      )
