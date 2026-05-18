package scraper.sites.chainedSoldier

import java.net.URI
import java.time.LocalDate

import scraper.domain.*

class ChainedSoldierScraperSuite extends scraper.ScraperSuite:

  test("Parse chained-soldier.live page"):
    readResourceHtml("sites/chained-soldier-live/chained-soldier.html").map:
      html =>
        ChainedSoldierScraper.parseContent(html) match
          case Left(error) => assert(false, error.toString)
          case Right(entries) =>
            assert(entries.nonEmpty)
            assertContains(
              entries,
              List(
                EntryFound(
                  EntryTitle("Chapter 179"),
                  EntryNo("179"),
                  EntryUri(
                    URI(
                      "https://chained-soldier.live/comic/chained-soldier-chapter-179/"
                    )
                  ),
                  DateUploaded(LocalDate.of(2026, 5, 3))
                ),
                EntryFound(
                  EntryTitle("Chapter 172.5"),
                  EntryNo("172.5"),
                  EntryUri(
                    URI(
                      "https://chained-soldier.live/comic/chained-soldier-chapter-172-5/"
                    )
                  ),
                  DateUploaded(LocalDate.of(2026, 1, 14))
                ),
                EntryFound(
                  EntryTitle("Chapter 1"),
                  EntryNo("1"),
                  EntryUri(
                    URI(
                      "https://chained-soldier.live/comic/chained-soldier-chapter-1/"
                    )
                  ),
                  DateUploaded(LocalDate.of(2025, 2, 10))
                )
              )
            )
