package scraper

import java.net.URI

import scrapeConfigs.sites.SiteScrapeConfig
import scraper.domain.{ EntryFound, JobLabel, ScrapeError }

trait Scraper[F[_]]:
  def scrape(instructions: List[(JobLabel, URI, SiteScrapeConfig[F])])
      : F[(List[(JobLabel, ScrapeError)], List[(JobLabel, EntryFound)])]
