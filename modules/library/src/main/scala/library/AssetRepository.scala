package library

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.implicits.*
import core.Tuples
import doobie.*
import doobie.implicits.*
import doobiex.*

import domain.*
import category.domain.CategoryId

trait AssetRepository[F[_]]:
  def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]]
  def findById(assetId: AssetId): F[Option[
    (
        ExistingAsset,
        List[ExistingAssetEntry]
    )
  ]]
  def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]]
  def add(entry: NewAssetEntry): F[Either[AddEntryError, ExistingAssetEntry]]
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
  private val findAllColumns =
    Columns(
      A(_.id),
      A(_.title),
      A(_.categoryId),
      AE(_.id).option,
      AE(_.no).option,
      AE(_.uri).option,
      AE(_.wasSeen).option,
      AE(_.dateUploaded).option
    )

  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): AssetRepository[F] = new:

    override def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]] =
      sql"""
          SELECT ${findAllColumns} 
          FROM ${A}
          LEFT JOIN ${AE} ON ${AE(_.assetId)} = ${A(_.id)}
          ORDER BY ${A(_.id)}
      """
        .queryOf(findAllColumns)
        .to[List]
        .transact(xa)
        .map: rows =>
          rows
            .groupBy(row => (row._1, row._2, row._3))
            .map: (asset, records) =>
              val (id, title, categoryId) = asset
              val entries = records
                .map: record =>
                  (
                    record._4,
                    record._5,
                    record._6,
                    record._7,
                    record._8,
                    id.some
                  ).tupled
                    .map(Tuples.from[ExistingAssetEntry](_))
                .collect:
                  case Some(entry) => entry
              ExistingAsset(id, title, categoryId) -> entries
            .toList
            .sortBy((asset, _) => asset.id)

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

    override def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]] =
      doesAssetExist(asset.title).flatMap:
        case true  => AddAssetError.AssetAlreadyExists.asLeft.pure
        case false => addWithoutChecking(asset).map(_.asRight)

    override def add(
        entry: NewAssetEntry
    ): F[Either[AddEntryError, ExistingAssetEntry]] =
      (doesAssetExist(entry.assetId), doesEntryExist(entry.uri)).tupled.flatMap:
        (assetExists, entryExists) =>
          if entryExists then AddEntryError.EntryAlreadyExists.asLeft.pure
          else if !assetExists then AddEntryError.AssetDoesNotExists.asLeft.pure
          else addWithoutChecking(entry).map(_.asRight)

    override def addToAsset(
        asset: ExistingAsset,
        categoryId: CategoryId
    ): F[Unit] =
      sql"""
      UPDATE ${Assets}
      SET ${Assets.categoryId === categoryId.some}
      WHERE ${Assets.id === asset.id}
      """.update.run.transact(xa).void

    override def update(asset: ExistingAsset): F[Unit] =
      sql"""
      UPDATE ${Assets}
      SET ${Assets.title === asset.title}
      WHERE ${Assets.id === asset.id}
      """.update.run.transact(xa).void

    override def update(entry: ExistingAssetEntry): F[Unit] =
      sql"""
      UPDATE ${AssetEntries}
      SET ${AssetEntries.no === entry.no},
        ${AssetEntries.uri === entry.uri},
        ${AssetEntries.wasSeen === entry.wasSeen},
        ${AssetEntries.dateUploaded === entry.dateUploaded}
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
      sql"INSERT INTO ${Assets}(${Assets.title}) VALUES (${asset.title}) RETURNING ${Assets.*}"
        .queryOf(Assets.*)
        .unique
        .transact(xa)
        .map: row =>
          Tuples.from[ExistingAsset](row)

    private def addWithoutChecking(
        entry: NewAssetEntry
    ): F[ExistingAssetEntry] =
      val values = Tuples.to(entry)
      sql"INSERT INTO ${AssetEntries}(${AssetEntries.allExceptId}) VALUES ($values) RETURNING ${AssetEntries.*}"
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
  val no           = Column[EntryNo]("no")
  val uri          = Column[EntryUri]("uri")
  val wasSeen      = Column[WasEntrySeen]("was_seen")
  val dateUploaded = Column[DateUploaded]("date_uploaded")
  val assetId      = Column[AssetId]("asset_id")

  val *           = Columns((id, no, uri, wasSeen, dateUploaded, assetId))
  val allExceptId = Columns((no, uri, wasSeen, dateUploaded, assetId))
