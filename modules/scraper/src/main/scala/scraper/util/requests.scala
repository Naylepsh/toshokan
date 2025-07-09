package scraper.util.requests

import java.net.URI

import cats.effect.kernel.Sync
import cats.syntax.all.*
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.Document

def getHtmlContent[F[_]: Sync](uri: URI): F[Either[Throwable, Document]] =
  val browser = JsoupBrowser()
  Sync[F].blocking(browser.get(uri.toString)).attempt
