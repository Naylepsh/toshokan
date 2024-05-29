package assetScraping

import java.net.URI

import scala.concurrent.duration.*
import scala.util.chaining.*

import cats.effect.kernel.{Clock, Sync}
import cats.syntax.all.*
import core.Measure.*
import library.AssetService
import library.domain.*
import scraper.domain.{EntryFound, JobLabel, SiteScraper}
import scraper.{Instruction, ScrapeJobSuccess, Scraper}

import domain.configs.*
import scrapes.domain.ScrapingSummary
import schedules.ScheduleService

trait AssetScrapingService[F[_]]:
  def findByAssetId(assetId: AssetId): F[Either[
    FindScrapingConfigError,
    (ExistingAsset, List[ExistingAssetScrapingConfig])
  ]]
  def add(
      scrapingConfig: NewAssetScrapingConfig
  ): F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]]
  def update(
      scrapingConfig: ExistingAssetScrapingConfig
  ): F[Either[UpdateScrapingConfigError, ExistingAssetScrapingConfig]]
  def delete(id: AssetScrapingConfigId): F[Unit]
  def getNewReleases: F[ScrapingSummary]
  def getNewReleasesAccordingToSchedule: F[ScrapingSummary]

object AssetScrapingService:
  def make[F[_]: Sync: Clock](
      repository: AssetScrapingRepository[F],
      assetService: AssetService[F],
      scheduleService: ScheduleService[F],
      scraper: Scraper[F],
      pickSiteScraper: Site => SiteScraper[F]
  ): AssetScrapingService[F] = new:
    def findByAssetId(assetId: AssetId): F[Either[
      FindScrapingConfigError,
      (ExistingAsset, List[ExistingAssetScrapingConfig])
    ]] =
      assetService
        .find(assetId)
        .flatMap:
          case Some(asset, _) =>
            repository
              .findByAssetId(assetId)
              .map: configs =>
                (asset, configs).asRight
          case None => FindScrapingConfigError.AssetDoesNotExists.asLeft.pure

    def add(
        scrapingConfig: NewAssetScrapingConfig
    ): F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]] =
      assetService
        .find(scrapingConfig.assetId)
        .flatMap:
          case Some(_) => repository.add(scrapingConfig)
          case None    => AddScrapingConfigError.AssetDoesNotExists.asLeft.pure

    def update(
        scrapingConfig: ExistingAssetScrapingConfig
    ): F[Either[UpdateScrapingConfigError, ExistingAssetScrapingConfig]] =
      assetService
        .find(scrapingConfig.assetId)
        .flatMap:
          case Some(_) => repository.update(scrapingConfig)
          case None => UpdateScrapingConfigError.AssetDoesNotExists.asLeft.pure

    def delete(id: AssetScrapingConfigId): F[Unit] =
      repository.delete(id)

    def getNewReleases: F[ScrapingSummary] =
      for
        configs <- repository.findAllEnabled
        instructions = makeInstructions(configs)
        results <- getNewReleases(instructions)
      yield results

    def getNewReleasesAccordingToSchedule: F[ScrapingSummary] =
      for
        assetIds <- scheduleService.findAssetsEligibleForScrape
        configs  <- repository.findAllEnabled
        instructions = makeInstructionsForAssets(assetIds, configs)
        results <- getNewReleases(instructions)
      yield results

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
          JobLabel(config.assetId.value),
          config.uri.value,
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
            JobLabel(config.assetId.value),
            config.uri.value,
            pickSiteScraper(config.site)
          )

    private def saveResults(successfulResults: List[ScrapeJobSuccess]) =
      successfulResults
        .flatMap: (label, entries) =>
          entries.map: entry =>
            NewAssetEntry.make(
              EntryNo(entry.no.value),
              EntryUri(entry.uri.value),
              DateUploaded(entry.dateUploaded.value),
              AssetId(label.value)
            )
        .pipe(assetService.addIfNewRelease)
        .measure
        .map: (results, savingTime) =>
          val newEntriesCount = results.foldLeft(0):
            case (newEntriesCount, Left(_)) =>
              newEntriesCount
            case (newEntriesCount, Right(savedEntries)) =>
              newEntriesCount + savedEntries.length
          (newEntriesCount, savingTime)
