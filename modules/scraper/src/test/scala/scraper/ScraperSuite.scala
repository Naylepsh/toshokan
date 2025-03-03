package scraper

import cats.effect.IO
import cats.effect.kernel.Resource
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

class ScraperSuite extends munit.CatsEffectSuite:
  val browser = JsoupBrowser()

  def readResourceHtml(filename: String) =
    readResourceFile(filename).map(browser.parseString)

  def readResourceFile(filename: String): IO[String] =
    Resource
      .fromAutoCloseable(IO(getClass.getResourceAsStream(s"/$filename")))
      .use(stream => IO(scala.io.Source.fromInputStream(stream).mkString))

  def assertContains[A](xs: List[A], x: A): Unit =
    assertEquals(xs.find(_ == x).isDefined, true, s"List does not contain $x")

  def assertContains[A](xs: List[A], ys: List[A]): Unit =
    ys.foreach(assertContains(xs, _))
