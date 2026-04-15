package scraper.sites.yatta

import scraper.domain.*

class YattaScraperSuite extends scraper.ScraperSuite:

  test("Parse yatta page"):
    readResourceHtml("sites/yatta/series-page.html").map: html =>
      YattaScraper.parseContent(html) match
        case Left(error) => fail(error.toString)
        case Right(entries) =>
          assertEquals(entries.size, 2)
          assert(entries.exists(_.title == EntryTitle("Manga #01")))
          assert(entries.exists(_.title == EntryTitle("Manga #04")))

  test("Return empty list when all products are unavailable"):
    readResourceHtml("sites/yatta/all-unavailable.html").map: html =>
      assertEquals(YattaScraper.parseContent(html), Right(Nil))

  test("Return error when no product containers found"):
    readResourceHtml("sites/yatta/empty-page.html").map: html =>
      assertEquals(
        YattaScraper.parseContent(html),
        Left(ScrapeError.NoEntriesFound)
      )
