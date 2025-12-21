package scraper.sites.batoto

import java.net.URI
import java.time.LocalDate

import scraper.domain.*

class BatotoScraperSuite extends scraper.ScraperSuite:
  import BatotoScraperSuite.*

  test("Parse manga page"):
    readResourceHtml("sites/bato-si/maids-three-star-cousine.html").map: html =>
      BatotoScraper.parseContent(html) match
        case Left(error) =>
          assert(false, error.toString)
        case Right(entries) =>
          assertContains(entries, expectedBatotoEntries)

object BatotoScraperSuite:
  private val expectedBatotoEntries = List(
    EntryFound(
      EntryTitle("Chapter 14.6"),
      EntryNo("14.6"),
      EntryUri(
        URI(
          "https://bato.si/title/161599-en-the-maid-s-three-star-cuisine-in-another-world-i-used-real-life-dishes-to-become-a-palace-sensation/3998379-ch_14.6"
        )
      ),
      DateUploaded(LocalDate.of(2025, 12, 16))
    ),
    EntryFound(
      EntryTitle("Chapter 14.5"),
      EntryNo("14.5"),
      EntryUri(
        URI(
          "https://bato.si/title/161599-en-the-maid-s-three-star-cuisine-in-another-world-i-used-real-life-dishes-to-become-a-palace-sensation/3952795-ch_14.5"
        )
      ),
      DateUploaded(LocalDate.of(2025, 11, 23))
    ),
    EntryFound(
      EntryTitle("Chapter 14.4"),
      EntryNo("14.4"),
      EntryUri(
        URI(
          "https://bato.si/title/161599-en-the-maid-s-three-star-cuisine-in-another-world-i-used-real-life-dishes-to-become-a-palace-sensation/3935027-ch_14.4"
        )
      ),
      DateUploaded(LocalDate.of(2025, 11, 16))
    )
  )
