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
          val titles = entries.map(_.title).toSet

          val expectedTitles = EntryTitle
            .applyAll("Chapter 14.6", "Chapter 14.5", "Chapter 14.4")
            .toSet
          assertEquals(titles, expectedTitles)

          // Check that entry numbers match
          val entryNos    = entries.map(_.no).toSet
          val expectedNos = EntryNo.applyAll("14.6", "14.5", "14.4").toSet
          assertEquals(entryNos, expectedNos)

          // Check that URLs are properly formed
          entries.foreach: entry =>
            assert(entry.uri.toString.contains("bato.si"))
            assert(
              entry.uri.toString
                .contains("161599-en-the-maid-s-three-star-cuisine")
            )

object BatotoScraperSuite
