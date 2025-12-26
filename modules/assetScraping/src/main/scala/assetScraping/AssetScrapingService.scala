package assetScraping

import scala.util.chaining.*

import assetScraping.configs.domain.FindScrapingConfigError
import cats.effect.kernel.{Clock, Sync}
import cats.mtl.Handle
import cats.syntax.all.*
import core.Measure.*
import library.AssetService
import library.category.domain.CategoryId
import library.domain.*
import neotype.*
import scraper.domain.{JobLabel, SiteScraper}
import scraper.{Instruction, ScrapeJobSuccess, Scraper}

import configs.AssetScrapingConfigService
import configs.domain.{Site, ExistingAssetScrapingConfig}
import schedules.ScheduleService
import scrapes.domain.ScrapingSummary

trait AssetScrapingService[F[_]]:
  def getNewReleases: F[ScrapingSummary]
  def getNewReleases(assetId: AssetId): F[ScrapingSummary]
  def getNewReleasesAccordingToSchedule: F[ScrapingSummary]
  def getNewReleasesOfCategory(
      categoryId: CategoryId
  ): F[Option[ScrapingSummary]]

object AssetScrapingService:
  def make[F[_]: Sync: Clock](
      assetService: AssetService[F],
      configService: AssetScrapingConfigService[F],
      scheduleService: ScheduleService[F],
      scraper: Scraper[F],
      pickSiteScraper: Site => SiteScraper[F]
  ): AssetScrapingService[F] = new:
    override def getNewReleases: F[ScrapingSummary] =
      for
        configs <- configService.findAllEnabled
        instructions = makeInstructions(configs)
        results <- getNewReleases(instructions)
      yield results

    override def getNewReleases(assetId: AssetId): F[ScrapingSummary] =
      for
        configs <- Handle
          .allow[FindScrapingConfigError]:
            configService.findByAssetId(assetId).map(_._2)
          .rescue:
            case FindScrapingConfigError.AssetDoesNotExists => List.empty.pure
        enabledConfigs = configs.filter(_.isEnabled)
        instructions   = makeInstructions(enabledConfigs)
        results <- getNewReleases(instructions)
      yield results

    override def getNewReleasesAccordingToSchedule: F[ScrapingSummary] =
      for
        assetIds <- scheduleService.findAssetsEligibleForScrape
        configs  <- configService.findAllEnabled
        instructions = makeInstructionsForAssets(assetIds, configs)
        results <- getNewReleases(instructions)
      yield results

    override def getNewReleasesOfCategory(
        categoryId: CategoryId
    ): F[Option[ScrapingSummary]] =
      assetService
        .matchCategoriesToAssets(categoryId :: Nil)
        .flatMap: matching =>
          matching
            .get(categoryId)
            .map: assetIds =>
              configService.findAllEnabled.flatMap: configs =>
                val instructions = makeInstructionsForAssets(assetIds, configs)
                getNewReleases(instructions).map(_.some)
            .getOrElse(None.pure)

    private def getNewReleases(instructions: List[Instruction[F]]) =
      for
        _ <- scribe.cats[F].info("Starting the asset scraping...")
        ((errors, successes), scrapingTime) <-
          scraper.scrape(instructions).measure
        (newEntriesCount, savingTime) <- saveResults(successes)
        _ = scribe.info("Done with the scrape")
        _ = errors.foreach(error => scribe.error(error.toString))
      yield ScrapingSummary(
        newEntriesCount,
        instructions.length,
        errors.length,
        scrapingTime.toSeconds,
        savingTime.toSeconds
      )

    private def makeInstructions(
        configs: List[ExistingAssetScrapingConfig]
    ): List[Instruction[F]] =
      configs.map: config =>
        (
          JobLabel(config.assetId.unwrap),
          config.uri,
          pickSiteScraper(config.site)
        )

    private def makeInstructionsForAssets(
        assetIds: List[AssetId],
        configs: List[ExistingAssetScrapingConfig]
    ) =
      configs
        .filter(config => assetIds.contains(config.assetId))
        .map: config =>
          (
            JobLabel(config.assetId.unwrap),
            config.uri,
            pickSiteScraper(config.site)
          )

    private def saveResults(successfulResults: List[ScrapeJobSuccess]) =
      successfulResults
        .flatMap: (label, entries) =>
          entries.map: entry =>
            NewAssetEntry.make(
              EntryTitle(entry.title),
              EntryNo(entry.no),
              EntryUri(entry.uri),
              DateUploaded(entry.dateUploaded),
              AssetId(label)
            )
        .pipe(assetService.addIfNewRelease)
        .measure
        .map: (results, savingTime) =>
          val newEntriesCount = results.foldLeft(0):
            case (newEntriesCount, Left(_)) =>
              newEntriesCount
            case (newEntriesCount, Right(savedEntry)) =>
              newEntriesCount + 1
          (newEntriesCount, savingTime)
