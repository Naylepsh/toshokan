package scraper.sites.empik

import java.net.URI
import java.time.LocalDate

import cats.effect.Sync
import cats.syntax.all.*
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.*
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.model.Document
import scraper.domain.*

class EmpikScraper[F[_]: Sync] extends SiteScraper[F]:
  override def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    getContent(uri).map:
      case Left(error)    => ScrapeError.Other(error.toString).asLeft
      case Right(content) => EmpikScraper.parseContent(content)

  private def getContent(uri: URI): F[Either[Throwable, Document]] =
    val browser = JsoupBrowser()
    Sync[F].blocking(browser.get(uri.toString)).attempt

object EmpikScraper:
  private val entryNoPattern = """.*Tom (\d+)""".r

  def parseContent(document: Document): Either[ScrapeError, List[EntryFound]] =
    val entries =
      document >> elementList("a.seoTitle[href]")
    val results = entries.traverse: entry =>
      val title   = EntryTitle(entry.text)
      val refLink = entry >> attr("href")
      EntryUri(s"https://www.empik.com${refLink}").map: uri =>
        val no = title match
          case entryNoPattern(rawNo) => Some(rawNo)
          case _                     => None
        // Empik does not store date uploaded so let's just default to "now"
        val dateUploaded = DateUploaded(LocalDate.now())

        EntryFound(title, EntryNo(no.getOrElse("")), uri, dateUploaded)

    results match
      case Left(error)    => ScrapeError.Other(error).asLeft
      case Right(Nil)     => ScrapeError.NoEntriesFound.asLeft
      case Right(entries) => entries.asRight
