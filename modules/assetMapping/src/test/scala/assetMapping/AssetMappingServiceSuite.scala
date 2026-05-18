package assetMapping

import java.net.URI
import java.time.LocalDate

import cats.effect.IO
import cats.mtl.Handle
import db.migrations.applyMigrations
import db.transactors.inMemoryTransactor
import doobie.util.transactor.Transactor
import library.asset.domain.*
import library.asset.{AssetRepository, AssetService}
import library.category.domain.{
  CategoryAlreadyExists,
  CategoryName,
  NewCategory
}
import library.category.{CategoryRepository, CategoryService}
import myAnimeList.domain.ExternalMangaId
import myAnimeList.{MyAnimeListClient, MyAnimeListServiceImpl}

class AssetMappingServiceSuite extends munit.CatsEffectSuite:
  import AssetMappingServiceSuite.*

  val withServices = ResourceFunFixture(
    inMemoryTransactor.evalTap(applyMigrations).evalMap(makeService)
  )

  withServices.test(
    "Assigning external id to manga fails when asset does not exist"
  ): (service, _, _) =>
    val result = Handle
      .allow[AssignExternalIdToMangaError]:
        service.assignExternalIdToManga(ExternalMangaId(1), AssetId(2))
      .rescue:
        case error: AssignExternalIdToMangaError => IO.pure(error)

    assertIO(result, AssetNotFound)

  withServices.test(
    "Assigning external id to manga fails when asset has no category"
  ): (service, assetService, _) =>
    val result = Handle
      .allow[AddAssetError | AssignExternalIdToMangaError]:
        for
          asset <- assetService.add(
            NewAsset(AssetTitle("test-asset"), None, Nil)
          )
          result <- service.assignExternalIdToManga(externalMangaId, asset.id)
        yield result
      .rescue:
        case error: (AddAssetError | AssignExternalIdToMangaError) =>
          IO.pure(error)

    assertIO(result, CategoryNotFound)

  withServices.test(
    "Assigning external id to manga fails when category is not a manga"
  ): (service, assetService, categoryService) =>
    val result = Handle
      .allow[
        CategoryAlreadyExists | AddAssetError | AssignExternalIdToMangaError
      ]:
        for
          category <- categoryService.add(NewCategory(CategoryName("anime")))
          asset <- assetService.add(
            NewAsset(AssetTitle("test-asset"), Some(category.id), Nil)
          )
          result <- service.assignExternalIdToManga(externalMangaId, asset.id)
        yield result
      .rescue:
        case error: (CategoryAlreadyExists | AddAssetError |
              AssignExternalIdToMangaError) =>
          IO.pure(error)

    assertIO(result, AssetIsNotManga)

  withServices.test(
    "Assigning external id to manga fails when asset already has an external id assigned"
  ): (service, assetService, categoryService) =>
    val result = Handle
      .allow[
        CategoryAlreadyExists | AddAssetError | AssignExternalIdToMangaError
      ]:
        for
          category <- categoryService.add(NewCategory(CategoryName("manga")))
          asset <- assetService.add(
            NewAsset(AssetTitle("test-asset1"), Some(category.id), Nil)
          )
          _ <- service.assignExternalIdToManga(ExternalMangaId(1), asset.id)
          result <- service.assignExternalIdToManga(
            ExternalMangaId(2),
            asset.id
          )
        yield result
      .rescue:
        case error: (CategoryAlreadyExists | AddAssetError |
              AssignExternalIdToMangaError) =>
          IO.pure(error)

    assertIO(result, MangaAlreadyHasExternalIdAssigned)

  withServices.test(
    "Assigning external id to manga fails when external id is already used by another asset"
  ): (service, assetService, categoryService) =>
    val result = Handle
      .allow[
        CategoryAlreadyExists | AddAssetError | AssignExternalIdToMangaError
      ]:
        for
          category <- categoryService.add(NewCategory(CategoryName("manga")))
          asset1 <- assetService.add(
            NewAsset(AssetTitle("test-asset1"), Some(category.id), Nil)
          )
          asset2 <- assetService.add(
            NewAsset(AssetTitle("test-asset2"), Some(category.id), Nil)
          )
          _      <- service.assignExternalIdToManga(externalMangaId, asset1.id)
          result <- service.assignExternalIdToManga(externalMangaId, asset2.id)
        yield result
      .rescue:
        case error: (CategoryAlreadyExists | AddAssetError |
              AssignExternalIdToMangaError) =>
          IO.pure(error)

    assertIO(result, ExternalIdAlreadyInUse)

object AssetMappingServiceSuite:
  val externalMangaId = ExternalMangaId(1)

  val entries = List(
    ExistingAssetEntry(
      EntryId(1),
      EntryTitle("The End of the Adventure"),
      EntryNo("1"),
      EntryUri(new URI("http://localhost:8080/foo/1")),
      DateUploaded(LocalDate.now()),
      AssetId(1)
    ),
    ExistingAssetEntry(
      EntryId(1),
      EntryTitle("The Priest's Lie"),
      EntryNo("2"),
      EntryUri(new URI("http://localhost:8080/foo/2")),
      DateUploaded(LocalDate.now()),
      AssetId(1)
    ),
    ExistingAssetEntry(
      EntryId(1),
      EntryTitle("Blue Moonweed"),
      EntryNo("3"),
      EntryUri(new URI("http://localhost:8080/foo/2")),
      DateUploaded(LocalDate.now()),
      AssetId(1)
    )
  )

  def makeService(
      xa: Transactor[IO]
  ): IO[(AssetMappingService, AssetService, CategoryService)] =
    val assetService    = AssetService.make(AssetRepository.make, xa)
    val categoryService = CategoryService.make(CategoryRepository.make, xa)
    MyAnimeListServiceImpl
      .make(xa, MyAnimeListClient.noop)
      .map: malService =>
        val service =
          AssetMappingService(
            assetService,
            categoryService,
            malService,
            MalMangaMappingRepository.make,
            xa
          )
        (service, assetService, categoryService)
