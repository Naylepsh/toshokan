package scraper

import java.net.URI

import cats.syntax.all.*
import cats.{Applicative, Parallel}
import scraper.domain.*

type ScrapeJobError   = (JobLabel, ScrapeError)
type ScrapeJobSuccess = (JobLabel, List[EntryFound])
type ScrapeResults    = (List[ScrapeJobError], List[ScrapeJobSuccess])
object ScrapeResults:
  val empty: ScrapeResults = (List.empty, List.empty)

type Instruction[F[_]] = (JobLabel, URI, SiteScraper[F])

trait Scraper[F[_]]:
  def scrape(instructions: List[Instruction[F]]): F[ScrapeResults]

object Scraper:
  def noop[F[_]: Applicative]: Scraper[F] = new:
    def scrape(
        instructions: List[Instruction[F]]
    ): F[ScrapeResults] =
      (List.empty, List.empty).pure

  def make[F[_]: Applicative: Parallel]: Scraper[F] = new:
    def scrape(instructions: List[Instruction[F]]): F[ScrapeResults] =
      instructions
        .groupBy(_._3)
        .toList
        .parTraverse: (_, instructions) =>
          instructions.traverse(findEntries.tupled)
        .map: results =>
          combineResults(results.flatten)

    private def findEntries(
        label: JobLabel,
        uri: URI,
        siteScraper: SiteScraper[F]
    ) =
      siteScraper
        .findEntries(uri)
        .map:
          case Left(reason)   => (label -> reason).asLeft
          case Right(entries) => (label -> entries).asRight

    private def combineResults(
        results: List[Either[
          ScrapeJobError,
          ScrapeJobSuccess
        ]]
    ) =
      results.foldLeft(ScrapeResults.empty):
        case ((errors, successes), result) =>
          result match
            case Left(error)    => (error :: errors) -> successes
            case Right(success) => errors            -> (success :: successes)
