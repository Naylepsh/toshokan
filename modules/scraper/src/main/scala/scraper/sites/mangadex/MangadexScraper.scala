package scraper.sites.mangadex

import java.net.URI

import cats.Monad
import cats.syntax.all.*
import scraper.domain.*

class MangadexScraper[F[_]: Monad](api: MangadexApi[F]) extends SiteScraper[F]:
  def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    MangadexScraper.extractMangaId(uri) match
      case Left(error) => error.asLeft.pure
      case Right(mangaId) =>
        api
          .getMangaFeed(mangaId)
          .map:
            case Left(error) => ScrapeError.Other(error.toString).asLeft
            case Right(feed) =>
              feed
                .data
                .map: chapter =>
                  EntryFound(
                    EntryNo(chapter.attributes.chapter),
                    EntryUri(chapter.url),
                    DateUploaded(chapter.attributes.createdAt.value)
                  )
                .asRight

object MangadexScraper:
  private val mangaIdFromUriPattern = "^https://mangadex.org/title/(.+)$".r

  def extractMangaId(uri: URI): Either[ScrapeError, String] =
    uri.toString match
      case mangaIdFromUriPattern(mangaId) => mangaId.asRight
      case _ =>
        ScrapeError
          .InvalidResource(s"Could not extract manga id from uri:$uri")
          .asLeft
