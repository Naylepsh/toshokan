package scraper

import java.net.URI

import cats.syntax.all.*
import cats.{ Applicative, Monad }
import scraper.domain.*

type ScrapeJobError   = (JobLabel, ScrapeError)
type ScrapeJobSuccess = (JobLabel, List[EntryFound])
type ScrapeResults    = (List[ScrapeJobError], List[ScrapeJobSuccess])
object ScrapeResults:
  val empty: ScrapeResults = (List.empty, List.empty)

trait Scraper[F[_]]:
  def scrape(instructions: List[(JobLabel, URI, SiteScraper[F])])
      : F[ScrapeResults]

object Scraper:
  def noop[F[_]: Applicative]: Scraper[F] = new:
    def scrape(instructions: List[(JobLabel, URI, SiteScraper[F])])
        : F[ScrapeResults] =
      (List.empty, List.empty).pure

  def make[F[_]: Monad]: Scraper[F] = new:
    def scrape(instructions: List[(JobLabel, URI, SiteScraper[F])])
        : F[ScrapeResults] =
      instructions
        .traverse: (label, uri, siteScraper) =>
          siteScraper.findEntries(uri).map:
            case Left(reason)   => (label -> reason).asLeft
            case Right(entries) => (label -> entries).asRight
        .map: results =>
          results.foldLeft(ScrapeResults.empty):
            case ((errors, successes), result) =>
              result match
                case Left(error)    => (error :: errors) -> successes
                case Right(success) => errors            -> (success :: successes)
