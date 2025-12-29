package scraper.sites.mangadex

import java.net.URI

import scala.concurrent.duration.DurationInt

import cats.Monad
import cats.effect.Temporal
import cats.syntax.all.*
import mangadex.MangadexApi
import mangadex.schemas.feed.GetMangaFeedResponse
import mangadex.utils.extractMangaId
import retry.{ResultHandler, RetryPolicies, retryingOnErrors}
import scraper.domain.*

class MangadexScraper[F[_]: Monad: Temporal](api: MangadexApi[F])
    extends SiteScraper[F]:
  def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    extractMangaId(uri).leftMap(ScrapeError.InvalidResource(_)) match
      case Left(error) => error.asLeft.pure
      case Right(mangaId) =>
        retryingOnErrors(api.getMangaFeed(mangaId))(
          policy = RetryPolicies
            .limitRetries(2)
            .join(RetryPolicies.exponentialBackoff(1.second)),
          errorHandler = ResultHandler.retryOnAllErrors(ResultHandler.noop)
        ).map:
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
            EntryTitle(
              chapter.attributes.title
                .getOrElse(s"Chapter ${chapter.attributes.chapter}")
            ),
            EntryNo(chapter.attributes.chapter),
            EntryUri(chapter.url),
            DateUploaded(chapter.attributes.createdAt)
          )
        .asRight
