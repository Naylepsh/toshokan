package scraper.sites.dynastyScans

import java.net.URI
import java.time.LocalDate

import scraper.domain.*

class DynastyScansScraperSuite extends scraper.ScraperSuite:
  import DynastyScansScraperSuite.*

  test("Parse manga page"):
    readResourceHtml("sites/dynasty-scans/bloom-into-you.html").map: html =>
      DynastyScansScraper.parseContent(html) match
        case Left(error) => assert(false, error.toString)
        case Right(entries) =>
          assertContains(entries, expectedDynstastyScansEntries)

object DynastyScansScraperSuite:
  private val expectedDynstastyScansEntries = List(
    EntryFound(
      EntryTitle("Chapter 42: Essay Question"),
      EntryNo("42"),
      EntryUri(
        URI("https://dynasty-scans.com/chapters/bloom_into_you_ch42")
      ),
      DateUploaded(LocalDate.of(2019, 6, 30))
    ),
    EntryFound(
      EntryTitle("Chapter 44: Night and Morning"),
      EntryNo("44"),
      EntryUri(
        URI("https://dynasty-scans.com/chapters/bloom_into_you_ch44")
      ),
      DateUploaded(LocalDate.of(2019, 9, 1))
    ),
    EntryFound(
      EntryTitle("Volume 6 Extras"),
      EntryNo(""),
      EntryUri(
        URI("https://dynasty-scans.com/chapters/bloom_into_you_volume_6_extras")
      ),
      DateUploaded(LocalDate.of(2018, 12, 15))
    )
  )
