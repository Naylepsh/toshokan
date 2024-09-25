package scraper.sites.mangadex

import java.net.URI

import cats.Monad
import cats.syntax.all.*
import scraper.domain.*

class MangadexScraper[F[_]: Monad](api: MangadexApi[F]) extends SiteScraper[F]:
  def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    // TODO: Add retries
    MangadexScraper.extractMangaId(uri) match
      case Left(error) => error.asLeft.pure
      case Right(mangaId) =>
        api
          .getMangaFeed(mangaId)
          .map:
            case Left(error) => ScrapeError.Other(error.toString).asLeft
            case Right(feed) => extractEntries(feed)

  private def extractEntries(feed: GetMangaFeedResponse) =
    if feed.data.isEmpty then ScrapeError.NoEntriesFound.asLeft
    else
      feed.data
        .filter: chapter =>
          // viz.com results are not viewable in PL
          chapter.url.getHost != "viz.com"
        .map: chapter =>
          EntryFound(
            EntryTitle(chapter.attributes.title),
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
