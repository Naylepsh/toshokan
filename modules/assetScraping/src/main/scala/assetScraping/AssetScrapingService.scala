package assetScraping

import cats.effect.kernel.{Clock, Sync}
import cats.syntax.all.*
import core.Measure.*
import library.AssetService
import library.domain.*
import scraper.Scraper
import scraper.domain.{EntryFound, JobLabel, SiteScraper}

import domain.*

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
        _       <- scribe.cats[F].info("Starting the asset scraping...")
        configs <- repository.findAllEnabled
        instructons = configs.map(makeScrapingInstruction)
        ((errors, successes), scrapingTime) <-
          scraper.scrape(instructons).measure
        newEntriesCount <- successes
          .traverse: (label, entries) =>
            entries.traverse(saveResult(label))
          .map: res =>
            res.flatten.foldLeft(0):
              case (newEntriesCount, Left(_)) =>
                newEntriesCount
              case (newEntriesCount, Right(_)) =>
                newEntriesCount + 1
        _ = scribe.info("Done with the scrape")
        _ = errors.foreach(error => scribe.error(error.toString))
      yield ScrapingSummary(
        newEntriesCount,
        errors.length,
        scrapingTime.toSeconds
      )

    private def makeScrapingInstruction(config: ExistingAssetScrapingConfig) =
      (
        JobLabel(config.assetId.value),
        config.uri.value,
        pickSiteScraper(config.site)
      )

    private def saveResult(label: JobLabel)(entry: EntryFound) =
      val newEntry = NewAssetEntry.make(
        EntryNo(entry.no.value),
        EntryUri(entry.uri.value),
        DateUploaded(entry.dateUploaded.value),
        AssetId(label.value)
      )
      assetService.add(newEntry)
