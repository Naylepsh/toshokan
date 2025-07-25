package scraper.sites.mangakakalot

import java.net.URI
import java.time.{LocalDate, LocalDateTime}

import cats.effect.Sync
import cats.syntax.all.*
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.dsl.DSL.*
import net.ruippeixotog.scalascraper.model.{Document, Element}
import scraper.domain.*
import scraper.util.requests.getHtmlContent

class MangakakalotScraper[F[_]: Sync] extends SiteScraper[F]:
  import MangakakalotScraper.*

  def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    Selectors(uri) match
      case None =>
        ScrapeError
          .InvalidResource(s"No selectors registered for ${uri}")
          .asLeft
          .pure
      case Some(selectors) =>
        getHtmlContent(uri).map:
          case Left(error)    => ScrapeError.Other(error.toString).asLeft
          case Right(content) => parseContent(content, selectors)

object MangakakalotScraper:
  def parseContent(
      document: Document,
      selectors: Selectors
  ): Either[ScrapeError, List[EntryFound]] =
    val chapterElems = (document >> elementList(selectors.chapters))
    if chapterElems.isEmpty then ScrapeError.NoEntriesFound.asLeft
    else
      chapterElems.traverse: chapterElem =>
        chapterElem >?> element(selectors.chapterName) match
          case None =>
            ScrapeError
              .Other(s"Invalid chapter selector ${selectors.chapterName}")
              .asLeft
          case Some(nameElem) =>
            val title        = extractTitle(nameElem)
            val no           = extractNo(nameElem)
            val uri          = extractUri(nameElem)
            val dateUploaded = extractDateUploaded(chapterElem, selectors)
            (no, uri, dateUploaded).tupled.map(EntryFound(title, _, _, _))

  private def extractTitle(nameElem: Element) = EntryTitle(nameElem.text)

  private val chapterNoPattern = ".*chapter[-_]([0-9]+[-]?[0-9]?).*".r
  private def extractNo(nameElem: Element) =
    (nameElem >?> attr("href")).map(_.split("/").last) match
      case Some(chapterNoPattern(name)) =>
        EntryNo(name.replaceAll("-", ".")).asRight
      case Some(chapterUrlResource) =>
        ScrapeError
          .Other(
            s"Chapter url resource ${chapterUrlResource} not match the pattern"
          )
          .asLeft
      case None =>
        ScrapeError
          .Other(s"Could not extract chapter no from text: ${nameElem.text}")
          .asLeft

  private def extractUri(nameElem: Element) =
    nameElem >?> attr("href") match
      case None =>
        ScrapeError.Other(s"Selector chapter has no href").asLeft
      case Some(href) => EntryUri(href).leftMap(ScrapeError.Other(_))

  private def extractDateUploaded(chapterElem: Element, selectors: Selectors) =
    (chapterElem
      >?> element(selectors.chapterTimeUploaded)
      >?> attr("title")).flatten match
      case None =>
        ScrapeError
          .Other(s"Selected chapter has no time uploaded")
          .asLeft
      case Some(timeUploaded) =>
        parseDateReleasedFromTimeUploaded(timeUploaded) match
          case None =>
            ScrapeError
              .Other(
                s"Could not parse chapter's time uploaded: ${timeUploaded}"
              )
              .asLeft
          case Some(dateUploaded) => DateUploaded(dateUploaded).asRight

  private val minutesAgoPattern = ".*([0-9]+) mins ago.*".r
  private val hoursAgoPattern   = ".*([0-9]+) hours? ago.*".r
  private val daysAgoPattern    = ".*([0-9]+) day ago.*".r
  private val mangakakalotDatePattern =
    ".*([A-Za-z]{3})-([0-9]{2})-([0-9]{4}).*".r
  private val manganatoDatePattern =
    ".*([A-Za-z]{3}) ([0-9]{2}),([0-9]{4}).*".r
  private def parseDateReleasedFromTimeUploaded(
      timeUploaded: String
  ): Option[LocalDate] =
    timeUploaded match
      case minutesAgoPattern(minutes) =>
        Some(LocalDateTime.now().minusMinutes(minutes.toInt).toLocalDate)

      case hoursAgoPattern(hours) =>
        Some(LocalDateTime.now().minusHours(hours.toInt).toLocalDate)

      case daysAgoPattern(days) =>
        Some(LocalDateTime.now().minusDays(days.toInt).toLocalDate)

      case mangakakalotDatePattern(month, day, year) =>
        composeDate(year, month, day)

      case manganatoDatePattern(month, day, year) =>
        composeDate(year, month, day)

      case _ => None

  private def composeDate(
      year: String,
      month: String,
      day: String
  ): Option[LocalDate] =
    for
      m <- monthWordToInt(month)
      d <- day.toIntOption
      y <- year.toIntOption.map: year =>
        if year >= 100 then year else 2000 + year
    yield LocalDate.of(y, m, d)

  private val months = List(
    "jan",
    "feb",
    "mar",
    "apr",
    "may",
    "jun",
    "jul",
    "aug",
    "sep",
    "oct",
    "nov",
    "dec"
  )
  private def monthWordToInt(monthWord: String): Option[Int] =
    months.indexOf(monthWord.toLowerCase) match
      case -1 => None
      case i  => Some(i + 1)
