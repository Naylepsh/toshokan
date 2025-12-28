package library

import cats.data.{NonEmptyList, OptionT}
import cats.effect.MonadCancelThrow
import cats.implicits.*
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import core.Tuples
import core.given
import db.extensions.*
import doobie.*
import doobie.implicits.*
import neotype.interop.doobie.given
import org.typelevel.cats.time.*

import domain.*
import category.domain.{CategoryId, CategoryName}
import category.Categories

trait AssetRepository[F[_]]:
  def findAll
      : F[List[(ExistingAsset, Option[CategoryName], List[ExistingAssetEntry])]]
  def findById(assetId: AssetId): F[Option[
    (
        ExistingAsset,
        List[ExistingAssetEntry]
    )
  ]]
  def findByEntryId(
      entryId: EntryId
  ): F[Option[(ExistingAsset, List[ExistingAssetEntry])]]
  def findStale(
      // TODO: this could be enforced to be a positive number
      minDaysToBeStale: Int
  ): F[List[(ExistingAsset, DateUploaded)]]
  def add(asset: NewAsset): Raise[F, AddAssetError] ?=> F[ExistingAsset]
  def add(
      entry: NewAssetEntry
  ): Raise[F, AddEntryError] ?=> F[ExistingAssetEntry]
  def addToAsset(asset: ExistingAsset, category: CategoryId): F[Unit]
  def update(asset: ExistingAsset): F[Unit]
  def update(entry: ExistingAssetEntry): F[Unit]
  def delete(assetId: AssetId): F[Unit]
  def matchCategoriesToAssets(
      categoryIds: NonEmptyList[CategoryId]
  ): F[Map[CategoryId, List[AssetId]]]

object AssetRepository:

  private val A  = Assets `as` "a"
  private val AE = AssetEntries `as` "ae"
  private val C  = Categories `as` "c"
  private val findAllColumns =
    Columns(
      A(_.id),
      A(_.title),
      A(_.categoryId),
      C(_.name_).option,
      AE(_.id).option,
      AE(_.title).option,
      AE(_.no).option,
      AE(_.uri).option,
      AE(_.dateUploaded).option
    )

  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): AssetRepository[F] = new:

    override def findAll: F[
      List[(ExistingAsset, Option[CategoryName], List[ExistingAssetEntry])]
    ] =
      sql"""
          SELECT ${findAllColumns} 
          FROM ${A}
          LEFT JOIN ${C} ON ${C(_.id)} = ${A(_.categoryId)}
          LEFT JOIN ${AE} ON ${AE(_.assetId)} = ${A(_.id)}
          ORDER BY ${A(_.id)}
      """
        .queryOf(findAllColumns)
        .to[List]
        .transact(xa)
        .map: rows =>
          rows
            .groupBy(row => (row._1, row._2, row._3, row._4))
            .map: (asset, records) =>
              val (id, title, categoryId, categoryName) = asset
              val entries = records
                .map: record =>
                  (
                    record._5,
                    record._6,
                    record._7,
                    record._8,
                    record._9,
                    id.some
                  ).tupled
                    .map(Tuples.from[ExistingAssetEntry](_))
                .collect:
                  case Some(entry) => entry
              (ExistingAsset(id, title, categoryId), categoryName, entries)
            .toList

    override def findById(assetId: AssetId): F[Option[
      (
          ExistingAsset,
          List[ExistingAssetEntry]
      )
    ]] =
      /** Ideally this would be done in one query, but I cba to do the sql ->
        * domain nested transformation
        */
      (
        findAsset(assetId),
        findEntries(assetId)
      ).tupled.map: (maybeAsset, entries) =>
        maybeAsset.map(asset => (asset, entries))

    override def findByEntryId(
        entryId: EntryId
    ): F[Option[(ExistingAsset, List[ExistingAssetEntry])]] =
      (for
        assetId <- OptionT(findAssetId(entryId))
        result  <- OptionT(findById(assetId))
      yield result).value

    override def findStale(
        minDaysToBeStale: Int
    ): F[List[(ExistingAsset, DateUploaded)]] =
      val cutoff =
        Fragment.const0(s"""date('now', '${minDaysToBeStale} day')""")
      // TODO: clean this workaround up
      sql"""
      SELECT ${A(_.*)}, MAX(${AE(_.dateUploaded)}) AS last_upload
      FROM ${A}
      -- ugly workaround here
      JOIN asset_scraping_configs ON asset_scraping_configs.asset_id = a.id
      AND asset_scraping_configs.is_enabled = 1
      -- end of the ugly workaround
      LEFT JOIN ${AE} ON ${AE(_.assetId)} = ${A(_.id)}
      GROUP BY ${A(_.id)}
      HAVING last_upload IS NULL OR date(last_upload) < ${cutoff}
      ORDER BY last_upload ASC
      """
        .query[(ExistingAsset, DateUploaded)]
        .to[List]
        .transact(xa)

    override def add(
        asset: NewAsset
    ): Raise[F, AddAssetError] ?=> F[ExistingAsset] =
      doesAssetExist(asset.title).flatMap:
        case true  => AssetAlreadyExists.raise
        case false => addWithoutChecking(asset)

    override def add(
        entry: NewAssetEntry
    ): Raise[F, AddEntryError] ?=> F[ExistingAssetEntry] =
      (doesAssetExist(entry.assetId), doesEntryExist(entry.uri)).tupled.flatMap:
        (assetExists, entryExists) =>
          if entryExists then AddEntryError.EntryAlreadyExists.raise
          else if !assetExists then AddEntryError.AssetDoesNotExists.raise
          else addWithoutChecking(entry)

    override def addToAsset(
        asset: ExistingAsset,
        categoryId: CategoryId
    ): F[Unit] =
      sql"""
      ${Assets.updateTableX(
          NonEmptyList.of(
            _.categoryId --> categoryId.some
          )
        )}
      WHERE ${Assets.id === asset.id}
      """.update.run.transact(xa).void

    override def update(asset: ExistingAsset): F[Unit] =
      sql"""
      ${Assets.updateTableX(NonEmptyList.of(_.title --> asset.title))}
      WHERE ${Assets.id === asset.id}
      """.update.run.transact(xa).void

    override def update(entry: ExistingAssetEntry): F[Unit] =
      sql"""
      ${AssetEntries.updateTableX(
          NonEmptyList.of(
            _.no --> entry.no,
            _.dateUploaded --> entry.dateUploaded
          )
        )}
      WHERE ${AssetEntries.id === entry.id}
      """.update.run.transact(xa).void

    override def delete(assetId: AssetId): F[Unit] =
      sql"DELETE FROM ${Assets} WHERE ${Assets.id} = ${assetId}".update.run
        .transact(xa)
        .void

    override def matchCategoriesToAssets(
        categoryIds: NonEmptyList[CategoryId]
    ): F[Map[CategoryId, List[AssetId]]] =
      val query = sql"""
      SELECT ${Assets.id}, ${Assets.categoryId}
      FROM ${Assets}
      WHERE """ ++ Fragments.in(Assets.categoryId, categoryIds)
      query
        .queryOf(Columns(Assets.id, Assets.categoryId))
        .to[List]
        .transact(xa)
        .map: rows =>
          rows
            .groupBy(_._2)
            .foldLeft(Map.empty):
              case (acc, (Some(categoryId), group)) =>
                acc + (categoryId -> group.map(_._1))
              case (acc, _) => acc

    private def findAsset(assetId: AssetId): F[Option[ExistingAsset]] =
      sql"""
        SELECT ${Assets.*}
        FROM ${Assets}
        WHERE ${Assets.id === assetId}
      """
        .queryOf(Assets.*)
        .option
        .transact(xa)
        .map: row =>
          row.map(Tuples.from[ExistingAsset](_))

    private def findAssetId(entryId: EntryId): F[Option[AssetId]] =
      sql"""
        SELECT ${AssetEntries.assetId}
        FROM ${AssetEntries}
        WHERE ${AssetEntries.id === entryId}
      """
        .queryOf(AssetEntries.assetId)
        .option
        .transact(xa)

    private def findEntries(assetId: AssetId): F[List[ExistingAssetEntry]] =
      sql"""
        SELECT ${AssetEntries.*} 
        FROM ${AssetEntries} 
        WHERE ${AssetEntries.assetId === assetId}
      """
        .queryOf(AssetEntries.*)
        .to[List]
        .transact(xa)
        .map: rows =>
          rows.map(Tuples.from[ExistingAssetEntry](_))

    private def addWithoutChecking(asset: NewAsset): F[ExistingAsset] =
      Assets
        .insertIntoReturning(
          NonEmptyList.of(
            _.title --> asset.title,
            _.categoryId --> asset.categoryId
          ),
          _.*
        )
        .queryOf(Assets.*)
        .unique
        .transact(xa)
        .map: row =>
          Tuples.from[ExistingAsset](row)

    private def addWithoutChecking(
        entry: NewAssetEntry
    ): F[ExistingAssetEntry] =
      AssetEntries
        .insertIntoReturning(
          NonEmptyList.of(
            _.title --> entry.title,
            _.no --> entry.no,
            _.uri --> entry.uri,
            _.dateUploaded --> entry.dateUploaded,
            _.assetId --> entry.assetId
          ),
          _.*
        )
        .queryOf(AssetEntries.*)
        .unique
        .transact(xa)
        .map(Tuples.from[ExistingAssetEntry](_))

    private def doesAssetExist(title: AssetTitle): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${Assets}
        WHERE ${Assets.title} = ${title}
      """.query[Int].option.transact(xa).map(_.isDefined)

    private def doesAssetExist(id: AssetId): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${Assets}
        WHERE ${Assets.id} = ${id}
      """.query[Int].option.transact(xa).map(_.isDefined)

    private def doesEntryExist(entryUri: EntryUri): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${AssetEntries}
        WHERE ${AssetEntries.uri === entryUri}
      """.query[Int].option.transact(xa).map(_.isDefined)

private object Assets extends TableDefinition("assets"):
  val id         = Column[AssetId]("id")
  val title      = Column[AssetTitle]("title")
  val categoryId = Column[Option[CategoryId]]("category_id")

  val * = Columns((id, title, categoryId))

private object AssetEntries extends TableDefinition("asset_entries"):
  val id           = Column[EntryId]("id")
  val title        = Column[EntryTitle]("title")
  val no           = Column[EntryNo]("no")
  val uri          = Column[EntryUri]("uri")
  val dateUploaded = Column[DateUploaded]("date_uploaded")
  val assetId      = Column[AssetId]("asset_id")

  val * = Columns((id, title, no, uri, dateUploaded, assetId))
