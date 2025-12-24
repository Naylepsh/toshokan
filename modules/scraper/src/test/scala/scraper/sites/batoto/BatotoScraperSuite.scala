package scraper.sites.batoto

import scraper.domain.*

class BatotoScraperSuite extends scraper.ScraperSuite:

  test("Parse manga page"):
    readResourceHtml("sites/bato-si/maids-three-star-cousine.html").map: html =>
      BatotoScraper.parseContent(html) match
        case Left(error) =>
          assert(false, error.toString)
        case Right(entries) =>
          // Check that we have the expected number of entries
          assertEquals(entries.length, 3)

          // Check that we have the expected chapters (ignoring dates)
          val titles = entries.map(_.title.value).toSet
          val expectedTitles =
            Set("Chapter 14.6", "Chapter 14.5", "Chapter 14.4")
          assertEquals(titles, expectedTitles)

          // Check that entry numbers match
          val entryNos    = entries.map(_.no.value).toSet
          val expectedNos = Set("14.6", "14.5", "14.4")
          assertEquals(entryNos, expectedNos)

          // Check that URLs are properly formed
          entries.foreach: entry =>
            assert(entry.uri.value.toString.contains("bato.si"))
            assert(
              entry.uri.value.toString
                .contains("161599-en-the-maid-s-three-star-cuisine")
            )

object BatotoScraperSuite
