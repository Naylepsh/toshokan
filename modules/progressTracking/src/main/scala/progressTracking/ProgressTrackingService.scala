package progressTracking

import cats.data.{NonEmptyList, OptionT}
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import doobie.ConnectionIO
import doobie.implicits.*
import doobie.util.fragment.Fragment
import doobie.util.query.Query0
import doobie.util.transactor.Transactor
import doobiex.*
import library.AssetService
import library.category.CategoryService
import library.domain.{AssetId, EntryNo, ExistingAssetEntry}
import progressTracking.mal.MyAnimeListClient

import util.chaining.*
import domain.*

/*
 * ProgressTracking module should probably sit on top of the library module?
 * Then the `wasSeen` should be probably moved from there to this module as well?
 * Thus this module needs a controller for handling these updates as well
 */

trait ProgressTrackingService[F[_]]:
  def searchForManga(term: Term): F[Either[String, List[Manga]]]
  def assignExternalIdToManga(
      externalId: ExternalMangaId,
      internalId: MangaId
  ): F[Unit]
  def updateProgress(assetId: AssetId, no: EntryNo): F[Either[Throwable, Unit]]

object ProgressTrackingService:

  def make[F[_]: MonadCancelThrow](
      xa: Transactor[F],
      malClient: MyAnimeListClient[F],
      assetService: AssetService[F],
      categoryService: CategoryService[F]
  ): ProgressTrackingService[F] = new:
    override def searchForManga(
        term: Term
    ): F[Either[String, List[Manga]]] =
      malClient
        .searchManga(term)
        .map: result =>
          result
            .leftMap(_.toString)
            .map: body =>
              body.data.map: d =>
                Manga(ExternalMangaId(d.node.id), MangaTitle(d.node.title))

    override def assignExternalIdToManga(
        externalId: ExternalMangaId,
        internalId: MangaId
    ): F[Unit] = MalMangaSql
      .assignMalIdToManga(externalId, internalId)
      .transact(xa)

    override def updateProgress(
        assetId: AssetId,
        no: EntryNo
    ): F[Either[Throwable, Unit]] =
      // TODO: This should also handle the `assetService.setSeen`

      val getMangaId = for
        (asset, entries) <- OptionT(assetService.find(assetId))
        category <- asset.categoryId
          .traverse(categoryService.find)
          .map(_.flatten)
          .pipe(OptionT(_))
        mangaId <- Option
          .when(isLatestEntry(no, entries))(MangaId(asset.id, category.name))
          .flatten
          .pipe(OptionT.fromOption(_))
      yield mangaId

      getMangaId.value
        .map((_, LatestChapter(no)).tupled)
        .flatMap(_.fold(Either.unit.pure)(malClient.updateStatus))

  /** EntryNo can be just a number, but it can also be a full chapter title. It
    * would probably make sense to force a split into separate EntryTitle and
    * EntryNo (which could default to 1 in the case of extraction failure)
    */
  def isLatestEntry(
      no: EntryNo,
      allEntries: List[ExistingAssetEntry]
  ): Boolean =
    no.value.toDoubleOption
      .map: noNumeric =>
        allEntries
          .mapFilter(_.no.value.toDoubleOption)
          .pipe(NonEmptyList.fromList)
          .fold(true)(_.maximum < noNumeric)
      .getOrElse(false)

private object MalMangaMapping extends TableDefinition("mal_manga_mapping"):
  val id      = Column[Long]("id")
  val mangaId = Column[MangaId]("manga_id")
  val malId   = Column[ExternalMangaId]("mal_id")

private object MalMangaSql:
  def assignMalIdToManga(
      externalId: ExternalMangaId,
      internalId: MangaId
  ): ConnectionIO[Unit] =
    insertInto(
      MalMangaMapping,
      NonEmptyList.of(_.mangaId --> internalId, _.malId --> externalId)
    ).update.run.void

  def findMalId(
      mangaId: MangaId
  ): ConnectionIO[Option[ExternalMangaId.Type]] =
    sql"""
    SELECT ${MalMangaMapping.malId}
    FROM ${MalMangaMapping}
    WHERE ${MalMangaMapping.mangaId === mangaId}"""
      .queryOf(MalMangaMapping.malId)
      .option
