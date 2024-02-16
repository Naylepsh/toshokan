package scraper

import java.net.URI

import scrapeConfigs.sites.SiteScrapeConfig
import scraper.domain.{ EntryFound, ScrapeError }

trait Scraper[F[_]]:
  def scrape(
      uri: URI,
      config: SiteScrapeConfig[F]
  ): F[Either[ScrapeError, List[EntryFound]]]
