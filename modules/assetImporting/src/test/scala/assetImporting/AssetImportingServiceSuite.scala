package assetImporting

import java.net.URI

import cats.effect.IO
import db.migrations.applyMigrations
import db.transactors.inMemoryTransactor
import doobie.Transactor
import doobie.implicits.*
import library.asset.domain.*
import library.asset.{AssetRepository, AssetService}
import library.author.AuthorRepository
import library.author.domain.AuthorName
import library.category.domain.{CategoryName, NewCategory}
import library.category.{CategoryRepository, CategoryService}

import testUtils.*

class AssetImportingServiceSuite extends munit.CatsEffectSuite:
  import AssetImportingServiceSuite.*

  val withService = ResourceFunFixture(
    inMemoryTransactor.evalTap(applyMigrations).evalMap(makeServices)
  )

  withService.test("imports asset with authors from Mangadex"):
    (service, assetService, authorRepo, xa) =>
      for
        asset   <- service.importFromMangadex(testMangadexUri)
        found   <- assetService.find(asset.id)
        authors <- authorRepo.findByIds(asset.authors).transact(xa)
      yield
        assertEquals(asset.title, AssetTitle("Test Manga"))
        assert(found.isDefined)
        assertEquals(
          authors.map(_.name).toSet,
          Set(AuthorName("Author One"), AuthorName("Artist Two"))
        )
        assertEquals(asset.authors.length, 2)

  withService.test("importing same manga twice reuses existing authors"):
    (service, _, authorRepo, xa) =>
      for
        asset1 <- service.importFromMangadex(testMangadexUri)
        // second import will fail because asset already exists,
        // but authors should have been created only once
        _ <- service.importFromMangadex(testMangadexUri2).attempt
        authors <- authorRepo
          .findOrAdd(
            Set(AuthorName("Author One"), AuthorName("Artist Two"))
          )
          .transact(xa)
      yield
        assertEquals(authors.size, 2)
        assertEquals(asset1.authors.length, 2)

  withService.test("imports asset with no authors when Mangadex returns none"):
    (service, assetService, _, _) =>
      for
        asset <- service.importFromMangadex(testMangadexUriNoAuthors)
        found <- assetService.find(asset.id)
      yield
        assertEquals(asset.title, AssetTitle("No Author Manga"))
        assert(found.isDefined)
        assertEquals(asset.authors, List.empty)

object AssetImportingServiceSuite:
  val testMangadexUri =
    domain.MangadexMangaUri(
      URI("https://mangadex.org/title/abc-123/test-manga")
    )
  val testMangadexUri2 =
    domain.MangadexMangaUri(
      URI("https://mangadex.org/title/abc-123/test-manga-duplicate")
    )
  val testMangadexUriNoAuthors =
    domain.MangadexMangaUri(
      URI("https://mangadex.org/title/no-authors/no-author-manga")
    )

  def makeServices(xa: Transactor[IO]) =
    val assetRepo       = AssetRepository.make
    val categoryRepo    = CategoryRepository.make
    val authorRepo      = AuthorRepository.make
    val assetService    = AssetService.make(assetRepo, xa)
    val categoryService = CategoryService.make(categoryRepo, xa)
    val configRepo =
      assetScraping.configs.AssetScrapingConfigRepository.make
    val configService =
      assetScraping.configs.AssetScrapingConfigService
        .make(configRepo, assetService, xa)
    for
      malService <- myAnimeList.MyAnimeListServiceImpl.make(xa, noopMalClient)
      mappingService = assetMapping.AssetMappingService(
        assetService,
        categoryService,
        malService,
        assetMapping.MalMangaMappingRepository.make,
        xa
      )
      mangadexApi = stubMangadexApi
      service = AssetImportingService(
        assetService,
        categoryService,
        mappingService,
        configService,
        authorRepo,
        mangadexApi,
        xa
      )
      _ <- cats.mtl.Handle
        .allow[library.category.domain.CategoryAlreadyExists]:
          categoryService.add(NewCategory(CategoryName("manga")))
        .rescue(_ =>
          IO.raiseError(RuntimeException("Failed to create manga category"))
        )
    yield (service, assetService, authorRepo, xa)
