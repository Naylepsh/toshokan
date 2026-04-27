package scraper.sites.mangadex

import java.net.URI

import scala.concurrent.duration.DurationInt

import cats.effect.IO
import cats.syntax.all.*
import mangadex.MangadexApi
import mangadex.schemas.feed.GetMangaFeedResponse
import mangadex.utils.extractMangaId
import retry.{ResultHandler, RetryPolicies, retryingOnErrors}
import scraper.domain.*

class MangadexScraper(api: MangadexApi) extends SiteScraper:
  def findEntries(uri: URI): IO[Either[ScrapeError, List[EntryFound]]] =
    extractMangaId(uri).leftMap(ScrapeError.InvalidResource(_)) match
      case Left(error) => IO.pure(error.asLeft)
      case Right(mangaId) =>
        retryingOnErrors(api.getMangaFeed(mangaId))(
          policy = RetryPolicies
            .limitRetries[IO](2)
            .join(RetryPolicies.exponentialBackoff[IO](1.second)),
          errorHandler = ResultHandler.retryOnAllErrors(ResultHandler.noop)
        ).map:
          case Left(error) => ScrapeError.Other(error.toString).asLeft
          case Right(feed) => extractEntries(feed)

  private def extractEntries(feed: GetMangaFeedResponse) =
    if feed.data.isEmpty then ScrapeError.NoEntriesFound.asLeft
    else
      feed.data
        .filter(_.url.getHost != "viz.com")
        .map: chapter =>
          val title = chapter.attributes.title
            .orElse(chapter.attributes.chapter.map(c => s"Chapter $c"))
            .getOrElse("?")
          EntryFound(
            EntryTitle(title),
            chapter.attributes.chapter.map(EntryNo(_)).getOrElse(EntryNo.empty),
            EntryUri(chapter.url),
            DateUploaded(chapter.attributes.createdAt)
          )
        .asRight
