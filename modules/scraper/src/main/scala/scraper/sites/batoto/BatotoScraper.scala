package scraper.sites.batoto

import java.net.URI
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, LocalDateTime}

import cats.effect.kernel.Sync
import cats.syntax.all.*
import net.ruippeixotog.scalascraper.dsl.DSL.*
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.model.Document
import scraper.domain.*
import scraper.util.requests.getHtmlContent

class BatotoScraper[F[_]: Sync] extends SiteScraper[F]:

  override def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    getHtmlContent(uri).map:
      case Left(error)    => ScrapeError.Other(error.toString).asLeft
      case Right(content) => BatotoScraper.parseContent(content)

object BatotoScraper:
  private val entryNoPattern = """(?:Volume \d+ )?Chapter (\d+(?:\.\d+)?)""".r
  private val dateReleasedPattern = """(\d+)\s+(days?|hours?|mins?)\s+ago""".r

  def parseContent(document: Document): Either[ScrapeError, List[EntryFound]] =
    (document >> elementList("[data-name='chapter-list'] .group > div"))
      .traverse: row =>
        val linkElement = row >> element("a.link-hover")
        val chapterText = linkElement.text

        val no = chapterText match
          case entryNoPattern(rawNo) => EntryNo(rawNo)
          case _                     => EntryNo("")

        val title   = EntryTitle(chapterText)
        val refLink = EntryUri(s"https://bato.si${linkElement.attr("href")}")

        val dateElement  = row >> element("span[data-passed]")
        val dateUploaded = parseDateUploaded(dateElement.text)

        (refLink, dateUploaded).tupled
          .map: (uri, dateUploaded) =>
            EntryFound(title, no, uri, dateUploaded)
          .leftMap(ScrapeError.Other(_))
      .flatMap:
        case Nil     => ScrapeError.NoEntriesFound.asLeft
        case entries => entries.asRight

  def parseDateUploaded(rawDate: String): Either[String, DateUploaded] =
    rawDate match
      case dateReleasedPattern(amountStr, unit) =>
        val amount         = amountStr.toLong
        val normalizedUnit = unit.toLowerCase.stripSuffix("s")
        normalizedUnit match
          case "min" =>
            Right(
              DateUploaded(
                LocalDateTime
                  .now()
                  .minus(amount, ChronoUnit.MINUTES)
                  .toLocalDate
              )
            )
          case "hour" =>
            Right(
              DateUploaded(
                LocalDateTime.now().minus(amount, ChronoUnit.HOURS).toLocalDate
              )
            )
          case "day" =>
            Right(DateUploaded(LocalDate.now().minus(amount, ChronoUnit.DAYS)))
          case _ => Left(s"Invalid unit: $unit")
      case _ => Left(s"Invalid date: $rawDate")
