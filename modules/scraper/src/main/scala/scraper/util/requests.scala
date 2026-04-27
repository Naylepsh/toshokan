package scraper.util.requests

import java.net.URI

import cats.effect.IO
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.Document

def getHtmlContent(uri: URI): IO[Either[Throwable, Document]] =
  val browser = JsoupBrowser()
  IO.blocking(browser.get(uri.toString)).attempt
