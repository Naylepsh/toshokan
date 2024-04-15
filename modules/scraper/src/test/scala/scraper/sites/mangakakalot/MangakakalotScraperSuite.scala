package scraper.sites.mangakakalot

import java.net.URI
import java.time.LocalDate

import com.github.nscala_time.time.Imports.*
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.Document
import org.joda.time.DateTime
import scraper.domain.*

import MangakakalotScraper.toJavaLocalDate

class MangakakalotScraperSuite extends munit.FunSuite:
  import MangakakalotScraperSuite.*

  test("Parse mangakakalot html"):
    val actual =
      MangakakalotScraper.parseContent(mangakakalotHtml, Selectors.mangakakalot)

    assertEquals(actual, Right(expectedMangakakalotEntries))

  test("Parse mangakakalot moved content"):
    val actual = MangakakalotScraper.parseContent(
      movedMangakakalotHtml,
      Selectors.mangakakalot
    )

    assertEquals(actual, Left(ScrapeError.NoEntriesFound))

  test("Parse manganato html"):
    val actual =
      MangakakalotScraper.parseContent(manganatoHtml, Selectors.manganato)

    assertEquals(actual, Right(expectedManganatoEntries))

object MangakakalotScraperSuite:
  private val browser = JsoupBrowser()

  val mangakakalotHtml: Document = browser.parseString("""
    | <div class="chapter-list">
    |   <div class="row">
    |     <span><a href="https://mangakakalot.com/chapter/ot927321/chapter_28" title="Karami Zakari: Boku no Honto to Kimi no Uso Vol.5 Chapter 28">Vol.5 Chapter 28</a></span>
    |     <span>0</span>
    |     <span title="1 hour ago ">1 hour ago </span>
    |   </div>
    |   <div class="row">
    |     <span><a href="https://mangakakalot.com/chapter/ot927321/chapter_27" title="Karami Zakari: Boku no Honto to Kimi no Uso Vol.5 Chapter 27">Vol.5 Chapter 27</a></span>
    |     <span>4,065</span>
    |     <span title="Sep-25-2022 05:25">Sep-25-22</span>
    |   </div>
    |   <div class="row">
    |     <span><a href="https://mangakakalot.com/chapter/ot927321/chapter_26.1" title="Karami Zakari: Boku no Honto to Kimi no Uso Vol.5 Chapter 26.1">Bonus</a></span>
    |     <span>4,065</span>
    |     <span title="Sep-25-2022 05:25">Sep-24-22</span>
    |   </div>
    | </div>
    """.stripMargin)
  val expectedMangakakalotEntries = List(
    EntryFound(
      EntryNo("28"),
      EntryUri(
        URI("https://mangakakalot.com/chapter/ot927321/chapter_28")
      ),
      DateUploaded((DateTime.now() - 1.hour).toJavaLocalDate)
    ),
    EntryFound(
      EntryNo("27"),
      EntryUri(
        URI("https://mangakakalot.com/chapter/ot927321/chapter_27")
      ),
      DateUploaded(LocalDate.of(2022, 9, 25))
    ),
    EntryFound(
      EntryNo("26.1"),
      EntryUri(
        URI("https://mangakakalot.com/chapter/ot927321/chapter_26.1")
      ),
      DateUploaded(LocalDate.of(2022, 9, 24))
    )
  )

  val manganatoHtml: Document = browser.parseString("""
    | <ul class="row-content-chapter">
    |  <li class="a-h">
    |    <a
    |      rel="nofollow"
    |      class="chapter-name text-nowrap"
    |      href="https://chapmanganato.to/manga-ib986110/chapter-40"
    |      title="I Favor the Villainess chapter Chapter 40: Dancing Maiden"
    |      >Chapter 40: Dancing Maiden</a
    |    >
    |    <span class="chapter-view text-nowrap">26.7K</span>
    |    <span class="chapter-time text-nowrap" title="Feb 17,2024 22:02"
    |      >Feb 17,24</span
    |    >
    |  </li>
    |  <li class="a-h">
    |    <a
    |      rel="nofollow"
    |      class="chapter-name text-nowrap"
    |      href="https://chapmanganato.to/manga-ib986110/chapter-39"
    |      title="I Favor the Villainess chapter Chapter 39: What's your name?"
    |      >Chapter 39: What's Your Name?</a
    |    >
    |    <span class="chapter-view text-nowrap">28.9K</span>
    |    <span class="chapter-time text-nowrap" title="Jan 19,2024 06:01"
    |      >Jan 19,24</span
    |    >
    |  </li>
    |  <li class="a-h">
    |    <a
    |      rel="nofollow"
    |      class="chapter-name text-nowrap"
    |      href="https://chapmanganato.to/manga-ib986110/chapter-38.1"
    |      title="I made up this chapter"
    |      >Extra</a
    |    >
    |    <span class="chapter-view text-nowrap">28.9K</span>
    |    <span class="chapter-time text-nowrap" title="Jan 19,2024 06:01"
    |      >Jan 19,24</span
    |    >
    |  </li>
    |</ul>""".stripMargin)
  val expectedManganatoEntries = List(
    EntryFound(
      EntryNo("40"),
      EntryUri(
        URI("https://chapmanganato.to/manga-ib986110/chapter-40")
      ),
      DateUploaded(LocalDate.of(2024, 2, 17))
    ),
    EntryFound(
      EntryNo("39"),
      EntryUri(
        URI("https://chapmanganato.to/manga-ib986110/chapter-39")
      ),
      DateUploaded(LocalDate.of(2024, 1, 19))
    ),
    EntryFound(
      EntryNo("38.1"),
      EntryUri(
        URI("https://chapmanganato.to/manga-ib986110/chapter-38.1")
      ),
      DateUploaded(LocalDate.of(2024, 1, 19))
    )
  )

  val movedMangakakalotHtml: Document = browser.parseString("""
    | <div class="login" style="min-height: 70px; color: black; padding-top: 20px; background: white">
    |   <br /><br /><br />
    |   <div style="margin-left: 20px">
    |      Sorry, the page you have requested cannot be found. Click
    |      <a style="color: blue" href="https://mangakakalot.com/">here</a> go visit
    |      our homepage
    |    </div>
    |  </div>""".stripMargin)
