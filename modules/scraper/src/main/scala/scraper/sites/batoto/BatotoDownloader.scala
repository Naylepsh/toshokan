package scraper.sites.batoto

import java.net.URI

import cats.effect.IO
import net.ruippeixotog.scalascraper.dsl.DSL.*
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.model.Document
import scraper.domain.EntryUri
import scraper.util.requests.getHtmlContent

class BatotoDownloader:
  def getImageUrls(entryUri: EntryUri): IO[List[URI]] =
    getHtmlContent(entryUri).flatMap:
      case Left(error)    => IO.raiseError(error)
      case Right(content) => IO.pure(BatotoDownloader.parseContent(content))

object BatotoDownloader:
  def parseContent(document: Document): List[URI] =
    (document >> elementList(".page-img[src]")).map: imageElem =>
      URI(imageElem >> attr("src"))
