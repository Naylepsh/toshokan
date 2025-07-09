package scraper.sites.dynastyScans

import java.net.URI
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale

import cats.effect.Sync
import cats.syntax.all.*
import net.ruippeixotog.scalascraper.dsl.DSL.*
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.model.Document
import scraper.domain.*
import scraper.util.requests.getHtmlContent

class DynastyScansScraper[F[_]: Sync] extends SiteScraper[F]:
  override def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    getHtmlContent(uri).map:
      case Left(error)    => ScrapeError.Other(error.toString).asLeft
      case Right(content) => DynastyScansScraper.parseContent(content)

object DynastyScansScraper:
  private val entryNoPattern = """Chapter (\d+).*""".r

  def parseContent(document: Document): Either[ScrapeError, List[EntryFound]] =
    val entries = document >> elementList(".chapter-list dd")
    val results = entries.traverse: entry =>
      val chapter = entry >> element("a.name[href]")
      val title   = EntryTitle(chapter.text)
      val refLink = chapter >> attr("href")
      val releaseDate =
        (entry >> element("small")).text.replaceFirst("released ", "")

      for
        uri          <- EntryUri(s"https://dynasty-scans.com${refLink}")
        dateUploaded <- parseDateUploaded(releaseDate)
        no = title match
          case entryNoPattern(rawNo) => Some(rawNo)
          case _                     => None
      yield EntryFound(title, EntryNo(no.getOrElse("")), uri, dateUploaded)

    results match
      case Left(error)    => ScrapeError.Other(error).asLeft
      case Right(Nil)     => ScrapeError.NoEntriesFound.asLeft
      case Right(entries) => entries.asRight

  private val formatter =
    DateTimeFormatter.ofPattern("MMM d ''yy", Locale.ENGLISH)

  def parseDateUploaded(rawDate: String): Either[String, DateUploaded] =
    Either
      .catchNonFatal(
        LocalDate.parse(rawDate.replaceAll("\\s+", " "), formatter)
      )
      .map(DateUploaded.apply)
      .leftMap(_.getMessage)
