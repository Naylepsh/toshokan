package scraper.sites.mangakakalot

import java.net.URI
import java.time.LocalDate

import scraper.domain.*

class MangakakalotScraperSuite extends scraper.ScraperSuite:
  import MangakakalotScraperSuite.*

  test("Parse mangakakalot page"):
    readResourceHtml("sites/mangakakalot-gg/after-school.html").map: html =>
      MangakakalotScraper.parseContent(html, Selectors.mangakakalot) match
        case Left(error) => assert(false, error.toString)
        case Right(entries) =>
          assertContains(entries, expectedMangakakalotEntries)

object MangakakalotScraperSuite:
  val expectedMangakakalotEntries = List(
    EntryFound(
      EntryTitle("Chapter 14"),
      EntryNo("14"),
      EntryUri(
        URI("https://www.mangakakalot.gg/manga/after-school/chapter-14")
      ),
      DateUploaded(LocalDate.of(2024, 10, 27))
    ),
    EntryFound(
      EntryTitle("Chapter 1"),
      EntryNo("1"),
      EntryUri(
        URI("https://www.mangakakalot.gg/manga/after-school/chapter-1")
      ),
      DateUploaded(LocalDate.of(2024, 10, 27))
    ),
    EntryFound(
      EntryTitle("Chapter 6.5"),
      EntryNo("6.5"),
      EntryUri(
        URI("https://www.mangakakalot.gg/manga/after-school/chapter-6-5")
      ),
      DateUploaded(LocalDate.of(2024, 10, 27))
    )
  )
