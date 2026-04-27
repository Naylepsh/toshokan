package scraper

import cats.effect.IO
import cats.syntax.all.*
import scraper.domain.*

trait Scraper:
  def scrape(instructions: List[Instruction]): IO[ScrapeResults]

object Scraper:
  def make: Scraper = new:
    def scrape(instructions: List[Instruction]): IO[ScrapeResults] =
      val (assetInstructions, authorInstructions) = instructions.foldMap:
        case i: Instruction.ScrapeAsset =>
          Map(i.scraper -> List(i)) -> Map.empty
        case i: Instruction.ScrapeAuthor =>
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

    private def findEntries(instruction: Instruction.ScrapeAsset) =
      instruction.scraper
        .findEntries(instruction.uri)
        .map(_.bimap(instruction.label -> _, instruction.label -> _))

    private def findAssets(instruction: Instruction.ScrapeAuthor) =
      instruction.scraper
        .scrapeForAssets(instruction.uri)
        .map(_.bimap(instruction.label -> _, instruction.label -> _))
