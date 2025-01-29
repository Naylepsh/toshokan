package scraper.sites.dynastyScans

import java.net.URI
import java.time.LocalDate

import cats.effect.IO
import cats.effect.kernel.Resource
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import scraper.domain.*

class DynastyScansScraperSuite extends munit.CatsEffectSuite:
  import DynastyScansScraperSuite.*

  test("Parse manga page"):
    readResourceHtml("sites/dynasty-scans/bloom-into-you.html").map: html =>
      DynastyScansScraper.parseContent(html) match
        case Left(error) => assert(false, error.toString)
        case Right(entries) =>
          assertContains(entries, expectedDynstastyScansEntries)

  private def assertContains[A](xs: List[A], x: A): Unit =
    assertEquals(xs.find(_ == x).isDefined, true, s"List does not contain $x")

  private def assertContains[A](xs: List[A], ys: List[A]): Unit =
    ys.foreach(assertContains(xs, _))

object DynastyScansScraperSuite:
  private val browser = JsoupBrowser()

  private def readResourceHtml(filename: String) =
    readResourceFile(filename).map(browser.parseString)

  private def readResourceFile(filename: String): IO[String] =
    Resource
      .fromAutoCloseable(IO(getClass.getResourceAsStream(s"/$filename")))
      .use(stream => IO(scala.io.Source.fromInputStream(stream).mkString))

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
