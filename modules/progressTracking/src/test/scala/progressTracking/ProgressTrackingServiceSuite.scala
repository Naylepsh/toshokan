package progressTracking

import java.net.URI
import java.time.LocalDate

import cats.effect.IO
import cats.syntax.all.*
import doobie.*
import doobie.util.transactor.Transactor
import library.category.{CategoryRepository, CategoryService}
import library.domain.*
import library.{AssetRepository, AssetService}
import myAnimeList.MyAnimeListServiceImpl
import myAnimeList.domain.ExternalMangaId
import assetMapping.AssetMappingService
import db.transactors.inMemoryTransactor
import db.migrations.applyMigrations

import testUtils.*

class ProgressTrackingServiceSuite extends munit.CatsEffectSuite:
  import ProgressTrackingServiceSuite.*

  val withServices = ResourceFunFixture(
    inMemoryTransactor[IO].evalTap(applyMigrations).evalMap(makeService)
  )

  test("isLatestEntry is false for non-numeric"):
    assertEquals(
      ProgressTrackingService
        .isLatestEntry(EntryNo("The last chapter"), entries),
      false
    )

  test("isLatestEntry is false for fractional"):
    assertEquals(
      ProgressTrackingService.isLatestEntry(EntryNo("42.5"), entries),
      false
    )

  test("isLatestEntry is false for middle entry"):
    assertEquals(
      ProgressTrackingService.isLatestEntry(EntryNo("2"), entries),
      false
    )

  test("isLatestEntry is false when there are no entries to compare to"):
    assertEquals(
      ProgressTrackingService.isLatestEntry(EntryNo("2"), List.empty),
      false
    )

  test("isLatestEntry is true when the entry exactly that of the latest"):
    assertEquals(
      ProgressTrackingService.isLatestEntry(EntryNo("3"), entries),
      true
    )

object ProgressTrackingServiceSuite:
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
  ): IO[(ProgressTrackingService[IO], AssetService[IO], CategoryService[IO])] =
    val assetService    = AssetService.make(AssetRepository.make(xa))
    val categoryService = CategoryService.make(CategoryRepository.make(xa))
    MyAnimeListServiceImpl
      .make(xa, noopMalClient)
      .map: malService =>
        val assetMappingService =
          AssetMappingService(assetService, categoryService, malService, xa)
        val service = ProgressTrackingService
          .make[IO](
            xa,
            malService,
            assetService,
            assetMappingService,
            categoryService
          )
        (service, assetService, categoryService)
