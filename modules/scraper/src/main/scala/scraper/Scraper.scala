package scraper

import java.net.URI

import cats.syntax.all.*
import cats.{ Applicative, Monad }
import cats.effect.Clock
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

  def make[F[_]: Monad](using clock: Clock[F]): Scraper[F] = new:
    def scrape(instructions: List[(JobLabel, URI, SiteScraper[F])])
        : F[ScrapeResults] =
      for
        startTime <- clock.monotonic
        results   <- instructions.traverse(findEntries.tupled).map(combineResults)
        endTime   <- clock.monotonic
        _ = scribe
          .info(
            s"Scraping took ${(endTime - startTime).toSeconds} seconds"
          )
      yield results

    private def findEntries(
        label: JobLabel,
        uri: URI,
        siteScraper: SiteScraper[F]
    ) =
      siteScraper.findEntries(uri).map:
        case Left(reason)   => (label -> reason).asLeft
        case Right(entries) => (label -> entries).asRight

    private def combineResults(results: List[Either[
      ScrapeJobError,
      ScrapeJobSuccess
    ]]) =
      results.foldLeft(ScrapeResults.empty):
        case ((errors, successes), result) =>
          result match
            case Left(error)    => (error :: errors) -> successes
            case Right(success) => errors            -> (success :: successes)
