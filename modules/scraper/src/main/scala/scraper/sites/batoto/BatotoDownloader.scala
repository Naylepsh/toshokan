package scraper.sites.batoto

import java.net.URI

import cats.effect.kernel.{MonadCancelThrow, Sync}
import cats.syntax.all.*
import net.ruippeixotog.scalascraper.dsl.DSL.*
import net.ruippeixotog.scalascraper.dsl.DSL.Extract.*
import net.ruippeixotog.scalascraper.model.Document
import scraper.domain.EntryUri
import scraper.util.requests.getHtmlContent

class BatotoDownloader[F[_]: Sync: MonadCancelThrow]:
  def getImageUrls(entryUri: EntryUri): F[List[URI]] =
    getHtmlContent(entryUri).flatMap:
      case Left(error)    => MonadCancelThrow[F].raiseError(error)
      case Right(content) => BatotoDownloader.parseContent(content).pure

object BatotoDownloader:
  def parseContent(document: Document): List[URI] =
    (document >> elementList(".page-img[src]")).map: imageElem =>
      URI(imageElem >> attr("src"))
