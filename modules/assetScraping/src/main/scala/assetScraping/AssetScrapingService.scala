package assetScraping

import scala.util.chaining.*

import assetScraping.configs.domain.FindScrapingConfigError
import cats.effect.IO
import cats.mtl.Handle
import cats.syntax.all.*
import core.Measure.*
import core.types.PositiveInt
import doobie.*
import doobie.implicits.*
import library.asset.domain.*
import library.asset.{AssetRepository, AssetService}
import library.author.AuthorRepository
import library.category.domain.CategoryId
import neotype.*
import neotype.interop.cats.given
import scraper.Scraper
import scraper.domain.*

import configs.{AssetScrapingConfigService, AuthorScrapingConfigService}
import configs.domain.{
  Site,
  AuthorSite,
  ExistingAssetScrapingConfig,
  ExistingAuthorScrapingConfig
}
import schedules.ScheduleService
import scrapes.domain.ScrapingSummary

trait AssetScrapingService:
  def getNewReleases: IO[ScrapingSummary]
  def getNewReleases(assetId: AssetId): IO[ScrapingSummary]
  def getNewReleasesAccordingToSchedule: IO[ScrapingSummary]
  def getNewReleasesOfCategory(
      categoryId: CategoryId
  ): IO[Option[ScrapingSummary]]
  def findStale
      : IO[List[(ExistingAsset, Option[library.asset.domain.DateUploaded])]]

object AssetScrapingService:
  def make(
      assetService: AssetService,
      assetRepository: AssetRepository,
      configService: AssetScrapingConfigService,
      authorConfigService: AuthorScrapingConfigService,
      authorRepository: AuthorRepository,
      scheduleService: ScheduleService,
      scraper: Scraper,
      pickAssetScraper: Site => SiteScraper,
      pickAuthorScraper: AuthorSite => SiteScraperOfAuthor,
      xa: Transactor[IO]
  ): AssetScrapingService = new:
    override def getNewReleases: IO[ScrapingSummary] =
      for
        assetConfigs  <- configService.findAllEnabled
        authorConfigs <- authorConfigService.findAllEnabled
        assetInstructions  = assetConfigs.map(makeAssetInstruction)
        authorInstructions = authorConfigs.map(makeAuthorInstruction)
        results <- getNewReleases(assetInstructions ++ authorInstructions)
      yield results

    override def getNewReleases(assetId: AssetId): IO[ScrapingSummary] =
      for
        configs <- Handle
          .allow[FindScrapingConfigError]:
            configService.findByAssetId(assetId).map(_._2)
          .rescue:
            case FindScrapingConfigError.AssetDoesNotExists => List.empty.pure
        instructions = configs.filter(_.isEnabled).map(makeAssetInstruction)
        results <- getNewReleases(instructions)
      yield results

    override def getNewReleasesAccordingToSchedule: IO[ScrapingSummary] =
      for
        assetIds     <- scheduleService.findAssetsEligibleForScrape
        assetConfigs <- configService.findAllEnabled
        isAuthorDay  <- scheduleService.isAuthorScrapeDay
        authorConfigs <-
          if isAuthorDay then authorConfigService.findAllEnabled
          else IO.pure(List.empty)
        assetInstructions  = makeInstructionsForAssets(assetIds, assetConfigs)
        authorInstructions = authorConfigs.map(makeAuthorInstruction)
        results <- getNewReleases(assetInstructions ++ authorInstructions)
      yield results

    override def getNewReleasesOfCategory(
        categoryId: CategoryId
    ): IO[Option[ScrapingSummary]] =
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
        : IO[List[(ExistingAsset, Option[library.asset.domain.DateUploaded])]] =
      for
        allStale <- assetRepository.findStale(PositiveInt(90)).transact(xa)
        enabledConfigs <- configService.findAllEnabled
        enabledAssetIds = enabledConfigs.map(_.assetId).toSet
        staleEnabled = allStale.filter: (asset, _) =>
          enabledAssetIds.contains(asset.id)
      yield staleEnabled

    private def getNewReleases(instructions: List[Instruction]) =
      for
        _ <- scribe.cats[IO].info("Starting the asset scraping...")
        (results, scrapingTime) <- scraper.scrape(instructions).measure
        (newEntriesCount, savingTime) <- saveEntries(
          results.successfulAssetJobs
        )
        (newEntriesOfAuthorsCount, assetSavingnTime) <- saveAssets(
          results.successfulAuthorJobs
        )
        _ = scribe.info("Done with the scrape")
        _ = results.failures.foreach(error => scribe.error(error.toString))
      yield ScrapingSummary(
        newEntriesCount + newEntriesOfAuthorsCount,
        instructions.length,
        results.failures.length,
        scrapingTime.toSeconds,
        (savingTime + assetSavingnTime).toSeconds
      )

    private def makeInstructionsForAssets(
        assetIds: List[AssetId],
        configs: List[ExistingAssetScrapingConfig]
    ): List[Instruction.ScrapeAsset] =
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
          .transact(xa)
        authorToId = authors.map(author => author.name -> author.id).toMap
        entryToAssetTitle = successfulResults
          .flatMap: (_, assets) =>
            assets.flatMap: asset =>
              asset.entries.toList.map(
                _ -> library.asset.domain.AssetTitle(asset.assetTitle)
              )
          .toMap
        assetsToAdd = successfulResults.flatMap: (_, assets) =>
          assets.map: asset =>
            val assetAuthors =
              asset.authors.map(library.author.domain.AuthorName.apply)
            NewAsset(
              library.asset.domain.AssetTitle(asset.assetTitle),
              None,
              assetAuthors
                .map(authorToId.get)
                .collect { case Some(id) => id }
                .toList
            )
        assets <- assetRepository.findOrAdd(assetsToAdd.toSet).transact(xa)
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
          .collect { case Some(entry) => entry }
          .pipe(entries => assetService.addIfNewRelease(entries.toList))
      yield result
      process.measure.map: (results, savingTime) =>
        val newEntriesCount = results.count(_.isRight)
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
          val newEntriesCount = results.count(_.isRight)
          (newEntriesCount, savingTime)

    private def makeAssetInstruction(
        config: ExistingAssetScrapingConfig
    ): Instruction.ScrapeAsset =
      Instruction.ScrapeAsset(
        JobLabel(config.assetId.unwrap),
        config.uri,
        pickAssetScraper(config.site)
      )

    private def makeAuthorInstruction(
        config: ExistingAuthorScrapingConfig
    ): Instruction.ScrapeAuthor =
      Instruction.ScrapeAuthor(
        JobLabel(config.authorId.unwrap),
        AuthorScrapingUri(config.uri),
        pickAuthorScraper(config.site)
      )
