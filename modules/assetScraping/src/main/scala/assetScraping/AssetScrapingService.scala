package assetScraping

import scala.util.chaining.*

import assetScraping.configs.domain.FindScrapingConfigError
import cats.effect.kernel.{Clock, Sync}
import cats.mtl.Handle
import cats.syntax.all.*
import core.Measure.*
import core.types.PositiveInt
import library.category.domain.CategoryId
import library.asset.domain.*
import library.asset.{AssetRepository, AssetService}
import neotype.*
import scraper.Scraper
import scraper.domain.*
import neotype.interop.cats.given

import configs.{AssetScrapingConfigService, AuthorScrapingConfigService}
import configs.domain.{
  Site,
  AuthorSite,
  ExistingAssetScrapingConfig,
  ExistingAuthorScrapingConfig
}
import schedules.ScheduleService
import scrapes.domain.ScrapingSummary
import library.author.AuthorRepository

trait AssetScrapingService[F[_]]:
  def getNewReleases: F[ScrapingSummary]
  def getNewReleases(assetId: AssetId): F[ScrapingSummary]
  def getNewReleasesAccordingToSchedule: F[ScrapingSummary]
  def getNewReleasesOfCategory(
      categoryId: CategoryId
  ): F[Option[ScrapingSummary]]
  def findStale: F[List[(ExistingAsset, library.asset.domain.DateUploaded)]]

object AssetScrapingService:
  def make[F[_]: Sync: Clock](
      assetService: AssetService[F],
      assetRepository: AssetRepository[F],
      configService: AssetScrapingConfigService[F],
      authorConfigService: AuthorScrapingConfigService[F],
      authorRepository: AuthorRepository[F],
      scheduleService: ScheduleService[F],
      scraper: Scraper[F],
      pickAssetScraper: Site => SiteScraper[F],
      pickAuthorScraper: AuthorSite => SiteScraperOfAuthor[F]
  ): AssetScrapingService[F] = new:
    override def getNewReleases: F[ScrapingSummary] =
      for
        assetConfigs  <- configService.findAllEnabled
        authorConfigs <- authorConfigService.findAllEnabled
        assetInstructions  = assetConfigs.map(makeAssetInstruction)
        authorInstructions = authorConfigs.map(makeAuthorInstruction)
        results <- getNewReleases(assetInstructions ++ authorInstructions)
      yield results

    override def getNewReleases(assetId: AssetId): F[ScrapingSummary] =
      for
        configs <- Handle
          .allow[FindScrapingConfigError]:
            configService.findByAssetId(assetId).map(_._2)
          .rescue:
            case FindScrapingConfigError.AssetDoesNotExists => List.empty.pure
        instructions = configs.filter(_.isEnabled).map(makeAssetInstruction)
        results <- getNewReleases(instructions)
      yield results

    override def getNewReleasesAccordingToSchedule: F[ScrapingSummary] =
      for
        assetIds      <- scheduleService.findAssetsEligibleForScrape
        assetConfigs  <- configService.findAllEnabled
        isAuthorDay   <- scheduleService.isAuthorScrapeDay
        authorConfigs <- if isAuthorDay then authorConfigService.findAllEnabled
                         else List.empty.pure
        assetInstructions  = makeInstructionsForAssets(assetIds, assetConfigs)
        authorInstructions = authorConfigs.map(makeAuthorInstruction)
        results <- getNewReleases(assetInstructions ++ authorInstructions)
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

    override def findStale
        : F[List[(ExistingAsset, library.asset.domain.DateUploaded)]] =
      for
        allStale       <- assetRepository.findStale(PositiveInt(90))
        enabledConfigs <- configService.findAllEnabled
        enabledAssetIds = enabledConfigs.map(_.assetId).toSet
        staleEnabled = allStale.filter: (asset, _) =>
          enabledAssetIds.contains(asset.id)
      yield staleEnabled

    private def getNewReleases(instructions: List[Instruction[F]]) =
      for
        _ <- scribe.cats[F].info("Starting the asset scraping...")
        (results, scrapingTime) <-
          scraper.scrape(instructions).measure
        (newEntriesCount, savingTime) <- saveEntries(
          results.successfulAssetJobs
        )
        // TODO: We should include the metric of how long it took to save author results
        // and the entry count of it as well
        _ <- saveAssets(results.successfulAuthorJobs)
        _ = scribe.info("Done with the scrape")
        _ = results.failures.foreach(error => scribe.error(error.toString))
      yield ScrapingSummary(
        newEntriesCount,
        instructions.length,
        results.failures.length,
        scrapingTime.toSeconds,
        savingTime.toSeconds
      )

    private def makeInstructionsForAssets(
        assetIds: List[AssetId],
        configs: List[ExistingAssetScrapingConfig]
    ): List[Instruction.ScrapeAsset[F]] =
      configs
        .filter(config => assetIds.contains(config.assetId))
        .map(makeAssetInstruction)

    private def saveAssets(successfulResults: List[SuccessfulJob[AssetFound]]) =
      val process = for
        authors <- successfulResults
          .flatMap: (_, assets) =>
            assets.flatMap: asset =>
              asset.authors.map(name => library.author.domain.AuthorName(name))
          .toSet
          .pipe(authorRepository.findOrAdd)
        authorToId = authors.map(author => author.name -> author.id).toMap
        entryToAssetTitle = successfulResults
          .flatMap: (_, assets) =>
            assets.flatMap: asset =>
              asset.entries.toList.map(
                _ -> library.asset.domain.AssetTitle(asset.assetTitle)
              )
          .toMap
        assetsToAdd = successfulResults.flatMap: (_, assets) =>
          assets
            .map: asset =>
              val assetAuthors =
                asset.authors.map(library.author.domain.AuthorName.apply)
              NewAsset(
                library.asset.domain.AssetTitle(asset.assetTitle),
                // TODO: What should the category be?
                None,
                assetAuthors
                  .map(authorToId.get)
                  .collect:
                    case Some(id) => id
                  .toList
              )
        assets <- assetRepository.findOrAdd(assetsToAdd.toSet)
        result <- entryToAssetTitle
          .map: (entry, assetTitle) =>
            assets
              .find(_.title.eqv(assetTitle))
              .map: asset =>
                NewAssetEntry.make(
                  library.asset.domain.EntryTitle(entry.title),
                  library.asset.domain.EntryNo(entry.no),
                  library.asset.domain.EntryUri(entry.uri),
                  library.asset.domain.DateUploaded(entry.dateUploaded),
                  asset.id
                )
          .collect:
            case Some(entry) => entry
          .pipe(entries => assetService.addIfNewRelease(entries.toList))
      yield result
      process.measure.map: (results, savingTime) =>
        val newEntriesCount = results.foldLeft(0):
          case (newEntriesCount, Left(_)) =>
            newEntriesCount
          case (newEntriesCount, Right(savedEntry)) =>
            newEntriesCount + 1
        (newEntriesCount, savingTime)

    private def saveEntries(
        successfulResults: List[SuccessfulJob[EntryFound]]
    ) =
      successfulResults
        .flatMap: (label, entries) =>
          entries.map: entry =>
            NewAssetEntry.make(
              library.asset.domain.EntryTitle(entry.title),
              library.asset.domain.EntryNo(entry.no),
              library.asset.domain.EntryUri(entry.uri),
              library.asset.domain.DateUploaded(entry.dateUploaded),
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

    private def makeAssetInstruction(
        config: ExistingAssetScrapingConfig
    ): Instruction.ScrapeAsset[F] =
      Instruction.ScrapeAsset[F](
        JobLabel(config.assetId.unwrap),
        config.uri,
        pickAssetScraper(config.site)
      )

    private def makeAuthorInstruction(
        config: ExistingAuthorScrapingConfig
    ): Instruction.ScrapeAuthor[F] =
      Instruction.ScrapeAuthor[F](
        JobLabel(config.authorId.unwrap),
        AuthorScrapingUri(config.uri),
        pickAuthorScraper(config.site)
      )
