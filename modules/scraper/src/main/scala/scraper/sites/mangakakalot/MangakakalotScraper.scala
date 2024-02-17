package scraper.sites.mangakakalot

import java.net.URI

import scraper.domain.{ EntryFound, ScrapeError, SiteScraper }

class MangakakalotScraper[F[_]] extends SiteScraper[F]:
  def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]] = ???
