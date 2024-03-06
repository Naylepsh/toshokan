package assetScraping

import cats.Monad
import cats.syntax.all.*
import library.AssetService
import library.domain.*
import scraper.Scraper
import scraper.domain.{ EntryFound, JobLabel, SiteScraper }

import domain.*

trait AssetScrapingService[F[_]]:
  def findByAssetId(assetId: AssetId): F[Either[
    FindScrapingConfigError,
    (ExistingAsset, List[ExistingAssetScrapingConfig])
  ]]
  def add(scrapingConfig: NewAssetScrapingConfig)
      : F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]]
  def delete(id: AssetScrapingConfigId): F[Unit]
  def scrapeAllEnabled: F[Unit]

object AssetScrapingService:
  def make[F[_]: Monad](
      repository: AssetScrapingRepository[F],
      assetService: AssetService[F],
      scraper: Scraper[F],
      pickSiteScraper: Site => SiteScraper[F]
  ): AssetScrapingService[F] = new:
    def findByAssetId(assetId: AssetId): F[Either[
      FindScrapingConfigError,
      (ExistingAsset, List[ExistingAssetScrapingConfig])
    ]] =
      assetService.find(assetId).flatMap:
        case Some(asset, _) => (asset, List.empty).asRight.pure
        case None           => FindScrapingConfigError.AssetDoesNotExists.asLeft.pure

    def add(scrapingConfig: NewAssetScrapingConfig)
        : F[Either[AddScrapingConfigError, ExistingAssetScrapingConfig]] =
      assetService.find(scrapingConfig.assetId).flatMap:
        case Some(_) => repository.add(scrapingConfig)
        case None    => AddScrapingConfigError.AssetDoesNotExists.asLeft.pure

    def delete(id: AssetScrapingConfigId): F[Unit] =
      repository.delete(id)

    def scrapeAllEnabled: F[Unit] =
      for
        configs <- repository.findAllEnabled
        instructons = configs.map(makeScrapingInstruction)
        results <- scraper.scrape(instructons)
        (errors, successes) = results
        _ <- successes.traverse: (label, entries) =>
          entries.traverse(saveResult(label))
        _ = errors.foreach(println)
      // TODO: log errors with proper logger
      yield ()

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
