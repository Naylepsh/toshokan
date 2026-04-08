package scraper

import cats.syntax.all.*
import cats.{Applicative, Parallel}
import scraper.domain.*

trait Scraper[F[_]]:
  def scrape(instructions: List[Instruction[F]]): F[ScrapeResults]

object Scraper:
  def make[F[_]: Applicative: Parallel]: Scraper[F] = new:
    def scrape(instructions: List[Instruction[F]]): F[ScrapeResults] =
      val (assetInstructions, authorInstructions) = instructions.foldMap:
        case i: Instruction.ScrapeAsset[F] =>
          Map(i.scraper -> List(i)) -> Map.empty
        case i: Instruction.ScrapeAuthor[F] =>
          Map.empty -> Map(i.scraper -> List(i))

      (
        assetInstructions.values.toList.parFlatTraverse(
          _.traverse(findEntries)
        ),
        authorInstructions.values.toList.parFlatTraverse(_.traverse(findAssets))
      ).parTupled.map: (assetResults, authorResults) =>
        val (assetFailures, assetSuccesses)   = assetResults.separate
        val (authorFailures, authorSuccesses) = authorResults.separate

        ScrapeResults(
          assetSuccesses,
          authorSuccesses,
          assetFailures ++ authorFailures
        )

    private def findEntries(instruction: Instruction.ScrapeAsset[F]) =
      instruction.scraper
        .findEntries(instruction.uri)
        .map(_.bimap(instruction.label -> _, instruction.label -> _))

    private def findAssets(instruction: Instruction.ScrapeAuthor[F]) =
      instruction.scraper
        .scrapeForAssets(instruction.uri)
        .map(_.bimap(instruction.label -> _, instruction.label -> _))
