package assetMapping

import java.net.URI
import java.time.LocalDate

import cats.data.EitherT
import cats.effect.IO
import db.migrations.applyMigrations
import db.transactors.inMemoryTransactor
import doobie.util.transactor.Transactor
import library.category.domain.{CategoryName, NewCategory}
import library.category.{CategoryRepository, CategoryService}
import library.domain.*
import library.{AssetRepository, AssetService}
import myAnimeList.MyAnimeListServiceImpl
import myAnimeList.domain.ExternalMangaId

import testUtils.noopMalClient

class AssetMappingServiceSuite extends munit.CatsEffectSuite:
  import AssetMappingServiceSuite.*

  val withServices = ResourceFunFixture(
    inMemoryTransactor[IO].evalTap(applyMigrations).evalMap(makeService)
  )

  withServices.test(
    "Assigning external id to manga fails when asset does not exist"
  ): (service, _, _) =>
    val result = service.assignExternalIdToManga(ExternalMangaId(1), AssetId(2))
    assertIO(result, Left(AssetNotFound))

  withServices.test(
    "Assigning external id to manga fails when asset has no category"
  ): (service, assetService, _) =>
    val result = for
      asset <- EitherT(
        assetService.add(NewAsset(AssetTitle("test-asset"), None))
      )
      result <- EitherT(
        service.assignExternalIdToManga(externalMangaId, asset.id)
      )
    yield result

    assertIO(result.value, Left(CategoryNotFound))

  withServices.test(
    "Assigning external id to manga fails when category is not a manga"
  ): (service, assetService, categoryService) =>
    val result = for
      category <- EitherT(
        categoryService.add(NewCategory(CategoryName("anime")))
      )
      asset <- EitherT(
        assetService.add(NewAsset(AssetTitle("test-asset"), Some(category.id)))
      )
      result <- EitherT(
        service.assignExternalIdToManga(externalMangaId, asset.id)
      )
    yield result

    assertIO(result.value, Left(AssetIsNotManga))

  withServices.test(
    "Assigning external id to manga fails when asset already has an external id assigned"
  ): (service, assetService, categoryService) =>
    val result = for
      category <- EitherT(
        categoryService.add(NewCategory(CategoryName("manga")))
      )
      asset <- EitherT(
        assetService.add(NewAsset(AssetTitle("test-asset1"), Some(category.id)))
      )
      _ <- EitherT(
        service.assignExternalIdToManga(ExternalMangaId(1), asset.id)
      )
      result <- EitherT(
        service.assignExternalIdToManga(ExternalMangaId(2), asset.id)
      )
    yield result

    assertIO(result.value, Left(MangaAlreadyHasExternalIdAssigned))

  withServices.test(
    "Assigning external id to manga fails when external id is already used by another asset"
  ): (service, assetService, categoryService) =>
    val result = for
      category <- EitherT(
        categoryService.add(NewCategory(CategoryName("manga")))
      )
      asset1 <- EitherT(
        assetService.add(NewAsset(AssetTitle("test-asset1"), Some(category.id)))
      )
      asset2 <- EitherT(
        assetService.add(NewAsset(AssetTitle("test-asset2"), Some(category.id)))
      )
      _ <- EitherT(
        service.assignExternalIdToManga(externalMangaId, asset1.id)
      )
      result <- EitherT(
        service.assignExternalIdToManga(externalMangaId, asset2.id)
      )
    yield result

    assertIO(result.value, Left(ExternalIdAlreadyInUse))

object AssetMappingServiceSuite:
  val externalMangaId = ExternalMangaId(1)

  val entries = List(
    ExistingAssetEntry(
      EntryId(1),
      EntryTitle("The End of the Adventure"),
      EntryNo("1"),
      EntryUri(new URI("http://localhost:8080/foo/1")),
      WasEntrySeen(false),
      DateUploaded(LocalDate.now()),
      AssetId(1)
    ),
    ExistingAssetEntry(
      EntryId(1),
      EntryTitle("The Priest's Lie"),
      EntryNo("2"),
      EntryUri(new URI("http://localhost:8080/foo/2")),
      WasEntrySeen(false),
      DateUploaded(LocalDate.now()),
      AssetId(1)
    ),
    ExistingAssetEntry(
      EntryId(1),
      EntryTitle("Blue Moonweed"),
      EntryNo("3"),
      EntryUri(new URI("http://localhost:8080/foo/2")),
      WasEntrySeen(false),
      DateUploaded(LocalDate.now()),
      AssetId(1)
    )
  )

  def makeService(
      xa: Transactor[IO]
  ): IO[(AssetMappingService[IO], AssetService[IO], CategoryService[IO])] =
    val assetService    = AssetService.make(AssetRepository.make(xa))
    val categoryService = CategoryService.make(CategoryRepository.make(xa))
    MyAnimeListServiceImpl
      .make(xa, noopMalClient)
      .map: malService =>
        val service =
          AssetMappingService(assetService, categoryService, malService, xa)
        (service, assetService, categoryService)
