package scraper.sites.mangadex

import java.net.URI

import scraper.domain.{ EntryFound, ScrapeError, SiteScraper }
import cats.Monad
import cats.syntax.all.*
import scraper.domain.EntryNo
import scraper.domain.EntryUri
import scraper.domain.DateUploaded
import java.time.LocalDate

class MangadexScraper[F[_]: Monad](api: MangadexApi[F]) extends SiteScraper[F]:
  def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    val result = MangadexScraper.extractMangaId(uri) match
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
                  chapter.attributes.externalUrl.map: url =>
                    val date = LocalDate.parse(chapter.attributes.createdAt)
                    EntryFound(
                      EntryNo(chapter.attributes.chapter),
                      EntryUri(URI(url)),
                      DateUploaded(date)
                    )
                .asRight
    result.as(???)

object MangadexScraper:
  def extractMangaId(uri: URI): Either[ScrapeError, String] = ???
