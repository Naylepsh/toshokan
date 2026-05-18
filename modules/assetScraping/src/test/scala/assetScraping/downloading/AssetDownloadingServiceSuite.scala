package assetScraping.downloading

import scala.concurrent.duration.*

import assetScraping.downloading.domain.DownloadDir
import cats.data.NonEmptyList
import cats.effect.IO
import cats.mtl.Handle
import cats.syntax.all.*
import core.types.PositiveInt
import db.migrations.applyMigrations
import db.transactors.inMemoryTransactor
import doobie.*
import library.asset.AssetRepository
import library.asset.domain.*
import library.author.domain.AuthorId
import library.category.domain.CategoryId
import mangadex.MangadexApi
import munit.CatsEffectSuite
import neotype.interop.cats.given
import sttp.client3.Response
import sttp.client3.impl.cats.implicits.*
import sttp.client3.testing.SttpBackendStub
import sttp.model.StatusCode
import sttp.monad.MonadError

trait TestAssetRepository extends AssetRepository:
  def findAll                                  = ???
  def findById(assetId: AssetId)               = ???
  def findByEntryId(entryId: EntryId)          = ???
  def findStale(minDaysToBeStale: PositiveInt) = ???
  def add(asset: NewAsset): ConnectionIO[Either[AddAssetError, ExistingAsset]] =
    ???
  def add(
      entry: NewAssetEntry
  ): ConnectionIO[Either[AddEntryError, ExistingAssetEntry]] = ???
  def addToAsset(
      asset: ExistingAsset,
      category: CategoryId
  ): ConnectionIO[Unit] = ???
  def update(asset: ExistingAsset): ConnectionIO[Unit]                     = ???
  def update(entry: ExistingAssetEntry): ConnectionIO[Unit]                = ???
  def delete(assetId: AssetId): ConnectionIO[Unit]                         = ???
  def matchCategoriesToAssets(categoryIds: NonEmptyList[CategoryId])       = ???
  def findOrAdd(assets: Set[NewAsset]): ConnectionIO[Set[ExistingAsset]]   = ???
  def mergeAsset(sourceId: AssetId, targetId: AssetId): ConnectionIO[Unit] = ???
  def findAssetsByAuthor(authorId: AuthorId)                               = ???
  def relinkAuthorAssets(
      sourceAssetId: AssetId,
      targetAuthorId: AuthorId
  ): ConnectionIO[Unit] = ???

class AssetDownloadingServiceSuite extends CatsEffectSuite:
  import AssetDownloadingServiceSuite.*

  val withXa = ResourceFunFixture(
    inMemoryTransactor.evalTap(applyMigrations)
  )

  withXa.test("downloadAll processes entries in order"): xa =>
    val entries = List(
      ExistingAssetEntry(
        EntryId(1),
        EntryTitle("Ch 1"),
        EntryNo("1"),
        EntryUri(java.net.URI("https://mangadex.org/chapter/test1")),
        DateUploaded(java.time.LocalDate.now()),
        AssetId(1)
      ),
      ExistingAssetEntry(
        EntryId(2),
        EntryTitle("Ch 2"),
        EntryNo("2"),
        EntryUri(java.net.URI("https://mangadex.org/chapter/test2")),
        DateUploaded(java.time.LocalDate.now()),
        AssetId(1)
      )
    )

    val mockRepo = new TestAssetRepository:
      override def findById(
          assetId: AssetId
      ): ConnectionIO[Option[(ExistingAsset, List[ExistingAssetEntry])]] =
        if assetId == AssetId(1) then
          FC.pure(
            Some(
              (
                ExistingAsset(AssetId(1), AssetTitle("Test"), None, List.empty),
                entries
              )
            )
          )
        else FC.pure(None)

      override def findByEntryId(
          entryId: EntryId
      ): ConnectionIO[Option[(ExistingAsset, List[ExistingAssetEntry])]] =
        entries.find(_.id === entryId) match
          case Some(entry) =>
            FC.pure(
              Some(
                (
                  ExistingAsset(
                    AssetId(1),
                    AssetTitle("Test"),
                    None,
                    List.empty
                  ),
                  List(entry)
                )
              )
            )
          case None => FC.pure(None)

    val mockMangadexApi = new MangadexApi:
      def getMangaFeed(mangaId: String) = ???
      def getManga(mangaId: String)     = ???
      def getImages(chapterId: String)  = IO.pure(Right(List.empty))

    val config  = AssetDownloadingService.Config(1.millis)
    val storage = EntryLocalStorage(DownloadDir.tmp)
    val service = AssetDownloadingService.make(
      mockMangadexApi,
      mockBackend,
      mockRepo,
      storage,
      config,
      xa
    )

    Handle
      .allow[DownloadError]:
        service
          .downloadAll(AssetId(1))
          .map: progress =>
            assertEquals(progress.totalEntries, 2)
            assertEquals(progress.completedEntries, 2)
            assertEquals(progress.failedEntries.size, 0)
            assert(progress.isComplete)
      .rescue: error =>
        IO(fail(s"Unexpected error: $error"))

  withXa.test("downloadAll handles missing asset"): xa =>
    val mockRepo = new TestAssetRepository:
      override def findById(
          assetId: AssetId
      ): ConnectionIO[Option[(ExistingAsset, List[ExistingAssetEntry])]] =
        FC.pure(None)

    val mockMangadexApi = new MangadexApi:
      def getMangaFeed(mangaId: String) = ???
      def getManga(mangaId: String)     = ???
      def getImages(chapterId: String)  = ???

    val config  = AssetDownloadingService.Config(1.millis)
    val storage = EntryLocalStorage(DownloadDir.tmp)
    val service = AssetDownloadingService.make(
      mockMangadexApi,
      mockBackend,
      mockRepo,
      storage,
      config,
      xa
    )

    Handle
      .allow[DownloadError]:
        service.downloadAll(AssetId(999)).map(_.asRight)
      .rescue(_.asLeft.pure)
      .map:
        case Left(DownloadError.AssetNotFound(id)) =>
          assertEquals(id, AssetId(999))
        case other => fail(s"Expected AssetNotFound, got $other")

object AssetDownloadingServiceSuite:
  val mockBackend = SttpBackendStub(implicitly[MonadError[IO]])
    .whenRequestMatches(_ => true)
    .thenRespond { (_: Any) =>
      IO(Response(Right(""), StatusCode.Ok, ""))
    }
