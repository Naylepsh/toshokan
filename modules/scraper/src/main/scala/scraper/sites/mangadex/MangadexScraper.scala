package scraper.sites.mangadex

import java.net.URI

import scraper.domain.{ EntryFound, ScrapeError, SiteScraper }

class MangadexScraper[F[_]](api: MangadexApi[F]) extends SiteScraper[F]:
  def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] =
    ???
