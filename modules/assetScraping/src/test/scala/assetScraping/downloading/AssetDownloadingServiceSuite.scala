package assetScraping.downloading

import scala.concurrent.duration.*

import assetScraping.downloading.domain.DownloadDir
import cats.data.NonEmptyList
import cats.effect.IO
import cats.mtl.Raise
import cats.syntax.eq.*
import library.AssetRepository
import library.category.domain.CategoryId
import library.domain.*
import mangadex.MangadexApi
import munit.CatsEffectSuite
import neotype.interop.cats.given
import sttp.client3.Response
import sttp.client3.impl.cats.implicits.*
import sttp.client3.testing.SttpBackendStub
import sttp.model.StatusCode
import sttp.monad.MonadError

trait TestAssetRepository extends AssetRepository[IO]:
  def findAll                                                              = ???
  def findById(assetId: AssetId)                                           = ???
  def findByEntryId(entryId: EntryId)                                      = ???
  def findStale(minDaysToBeStale: Int)                                     = ???
  def add(asset: NewAsset): Raise[IO, AddAssetError] ?=> IO[ExistingAsset] = ???
  def add(
      entry: NewAssetEntry
  ): Raise[IO, AddEntryError] ?=> IO[ExistingAssetEntry] = ???
  def addToAsset(asset: ExistingAsset, category: CategoryId): IO[Unit] = ???
  def update(asset: ExistingAsset): IO[Unit]                           = ???
  def update(entry: ExistingAssetEntry): IO[Unit]                      = ???
  def delete(assetId: AssetId): IO[Unit]                               = ???
  def matchCategoriesToAssets(
      categoryIds: NonEmptyList[CategoryId]
  ): IO[Map[CategoryId, List[AssetId]]] = ???

class AssetDownloadingServiceSuite extends CatsEffectSuite:
  import AssetDownloadingServiceSuite.*

  test("downloadAll processes entries in order"):
    val entries = List(
      ExistingAssetEntry(
        EntryId(1),
        EntryTitle("Ch 1"),
        EntryNo("1"),
        EntryUri(java.net.URI("https://mangadex.org/chapter/test1")),
        WasEntrySeen(false),
        DateUploaded(java.time.LocalDate.now()),
        AssetId(1)
      ),
      ExistingAssetEntry(
        EntryId(2),
        EntryTitle("Ch 2"),
        EntryNo("2"),
        EntryUri(java.net.URI("https://mangadex.org/chapter/test2")),
        WasEntrySeen(false),
        DateUploaded(java.time.LocalDate.now()),
        AssetId(1)
      )
    )

    val mockRepo = new TestAssetRepository:
      override def findById(
          assetId: AssetId
      ): IO[Option[(ExistingAsset, List[ExistingAssetEntry])]] =
        if assetId == AssetId(1) then
          IO.pure(
            Some((ExistingAsset(AssetId(1), AssetTitle("Test"), None), entries))
          )
        else IO.pure(None)

      override def findByEntryId(
          entryId: EntryId
      ): IO[Option[(ExistingAsset, List[ExistingAssetEntry])]] =
        entries.find(_.id === entryId) match
          case Some(entry) =>
            IO.pure(
              Some(
                (
                  ExistingAsset(AssetId(1), AssetTitle("Test"), None),
                  List(entry)
                )
              )
            )
          case None => IO.pure(None)

    val mockMangadexApi = new MangadexApi[IO]:
      def getMangaFeed(mangaId: String) = ???
      def getManga(mangaId: String)     = ???
      def getImages(chapterId: String)  = IO.pure(Right(List.empty))

    val config = AssetDownloadingService.Config(1.millis)
    val service = AssetDownloadingService.make(
      mockMangadexApi,
      mockBackend,
      DownloadDir.tmp,
      mockRepo,
      config
    )

    service
      .downloadAll(AssetId(1))
      .map: progress =>
        assertEquals(progress.totalEntries, 2)
        assertEquals(progress.completedEntries, 2)
        assertEquals(progress.failedEntries.size, 0)
        assert(progress.isComplete)

  test("downloadAll handles missing asset"):
    val mockRepo = new TestAssetRepository:
      override def findById(
          assetId: AssetId
      ): IO[Option[(ExistingAsset, List[ExistingAssetEntry])]] = IO.pure(None)

    val mockMangadexApi = new MangadexApi[IO]:
      def getMangaFeed(mangaId: String) = ???
      def getManga(mangaId: String)     = ???
      def getImages(chapterId: String)  = ???

    val config = AssetDownloadingService.Config(1.millis)
    val service = AssetDownloadingService.make(
      mockMangadexApi,
      mockBackend,
      DownloadDir.tmp,
      mockRepo,
      config
    )

    interceptIO[AssetNotFound]:
      service.downloadAll(AssetId(999))

object AssetDownloadingServiceSuite:
  val mockBackend = SttpBackendStub(implicitly[MonadError[IO]])
    .whenRequestMatches(_ => true)
    .thenRespond { (_: Any) =>
      IO(Response(Right(""), StatusCode.Ok, ""))
    }
