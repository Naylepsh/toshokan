package progressTracking

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.LocalDate

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.all.*
import db.transactors.makeSqliteTransactorResource
import db.{Config, Path}
import doobie.*
import doobie.implicits.*
import doobie.util.transactor.Transactor
import library.category.domain.{CategoryName, NewCategory}
import library.category.{CategoryRepository, CategoryService}
import library.domain.*
import library.{AssetRepository, AssetService}
import sttp.model.Uri

import domain.*
import mal.*

class ProgressTrackingServiceSuite extends munit.CatsEffectSuite:
  import ProgressTrackingServiceSuite.*

  val withServices = ResourceFunFixture(
    inMemoryTransactor.evalTap(applyMigrations).evalMap(makeService)
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

  val inMemoryTransactor =
    makeSqliteTransactorResource[IO](Config.forSqlite(Path("sqlite::memory:")))

  def makeService(
      xa: Transactor[IO]
  ): IO[(ProgressTrackingService[IO], AssetService[IO], CategoryService[IO])] =
    val assetService    = AssetService.make(AssetRepository.make(xa))
    val categoryService = CategoryService.make(CategoryRepository.make(xa))
    ProgressTrackingService
      .make[IO](xa, noopMalClient, assetService, categoryService)
      .map: service =>
        (service, assetService, categoryService)

  val noopMalClient: MyAnimeListClient[IO] = new:
    override def generateCodeChallenge: IO[String] = IO.pure("CH4LL3NG3")
    override def createAuthorizationLink(codeChallenge: String): Uri = ???
    override def refreshAuthToken(
        token: RefreshToken
    ): IO[Either[Throwable, AuthToken]] = IO.pure(
      Right(
        AuthToken(
          0L,
          RefreshToken("r3fr3sh-tok3n"),
          AccessToken("4cc355-t0k3n")
        )
      )
    )
    override def acquireToken(
        code: String,
        codeChallenge: String
    ): IO[Either[Throwable, AuthToken]] = IO.pure(
      Right(
        AuthToken(
          0L,
          RefreshToken("r3fr3sh-tok3n"),
          AccessToken("4cc355-t0k3n")
        )
      )
    )
    override def searchManga(
        token: AuthToken,
        term: Term
    ): IO[Either[Throwable, GetMangaListSuccess]] = ???
    override def updateStatus(
        token: AuthToken,
        mangaId: ExternalMangaId,
        latestChapter: LatestChapter
    ): IO[Either[Throwable, Unit]] = ???

  val migrationSQL = IO:
    new String(
      Files.readAllBytes(Paths.get("./db/schema.sql")),
      StandardCharsets.UTF_8
    )

  def applyMigrations(xa: Transactor[IO]): IO[Unit] =
    migrationSQL.flatMap: sql =>
      sql
        .split(";")
        .toList
        .filterNot(_.isBlank)
        .traverse: statement =>
          Fragment.const(statement).update.run
        .transact(xa)
        .void
