package scraper

import java.net.URI

import scraper.domain.{ EntryFound, JobLabel, ScrapeError, SiteScraper }
import cats.Applicative
import cats.syntax.all.*

trait Scraper[F[_]]:
  def scrape(instructions: List[(JobLabel, URI, SiteScraper[F])])
      : F[(List[(JobLabel, ScrapeError)], List[(JobLabel, EntryFound)])]

object Scraper:
  def noop[F[_]: Applicative]: Scraper[F] = new:
    def scrape(instructions: List[(JobLabel, URI, SiteScraper[F])])
        : F[(List[(JobLabel, ScrapeError)], List[(JobLabel, EntryFound)])] =
      (List.empty, List.empty).pure
