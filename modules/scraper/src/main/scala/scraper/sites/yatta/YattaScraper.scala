package scraper.sites.yatta

import java.net.URI
import java.time.LocalDate

import cats.effect.Sync
import cats.syntax.all.*
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.dsl.DSL.*
import net.ruippeixotog.scalascraper.model.Document
import scraper.domain.*

class YattaScraper[F[_]: Sync] extends SiteScraper[F]:
  override def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    getContent(uri).map:
      case Left(error)    => ScrapeError.Other(error.toString).asLeft
      case Right(content) => YattaScraper.parseContent(content)

  private def getContent(uri: URI): F[Either[Throwable, Document]] =
    val browser = JsoupBrowser()
    Sync[F].blocking(browser.get(uri.toString)).attempt

object YattaScraper:
  def parseContent(document: Document): Either[ScrapeError, List[EntryFound]] =
    val entries =
      document >> elementList("#katalog_content #product_container_large")
    val results = entries.traverse: entry =>
      val href = entry >> element("a") >> attr("href")
      EntryUri(s"https:${href}").map: uri =>
        val no = EntryNo((entry >> element(".product-title")).text)
        // Yatta does not store date uploaded so let's just default to "now"
        val dateUploaded = DateUploaded(LocalDate.now())

        EntryFound(no, uri, dateUploaded)

    results match
      case Left(error) => ScrapeError.Other(error).asLeft
      case Right(Nil)  => ScrapeError.NoEntriesFound.asLeft
      case Right(entries) =>
        entries.filter(!_.no.startsWith("Prenumerata")).asRight
