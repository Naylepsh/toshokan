package assetScraping

import scala.util.chaining.*
import scala.concurrent.duration.*

import cats.effect.kernel.{Clock, Sync}
import cats.syntax.all.*
import core.Measure.*
import library.AssetService
import library.domain.*
import scraper.Scraper
import scraper.domain.{EntryFound, JobLabel, SiteScraper}

import domain.*
import scraper.ScrapeJobSuccess

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

object AssetScrapingService:
  def make[F[_]: Sync: Clock](
      repository: AssetScrapingRepository[F],
      assetService: AssetService[F],
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
        _            <- scribe.cats[F].info("Starting the asset scraping...")
        instructions <- getScrapingInstructions
        ((errors, successes), scrapingTime) <-
          scraper.scrape(instructions).measure
        (newEntriesCount, savingTime) <- saveResults(successes)
        _ = scribe.info("Done with the scrape")
        _ = errors.foreach(error => scribe.error(error.toString))
      yield ScrapingSummary(
        newEntriesCount,
        errors.length,
        scrapingTime.toSeconds,
        savingTime.toSeconds
      )

    private def getScrapingInstructions =
      repository.findAllEnabled.map: configs =>
        configs.map: config =>
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
