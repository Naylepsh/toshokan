package library

import cats.effect.MonadCancelThrow
import cats.implicits.*
import core.Tuples
import doobie.*
import doobie.implicits.*
import doobiex.*
import library.domain.*

trait AssetRepository[F[_]]:
  def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]]
  def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]]
  def addEntry(entry: NewAssetEntry)
      : F[Either[AddEntryError, ExistingAssetEntry]]
  def delete(assetId: AssetId): F[Unit]

object AssetRepository:
  object Assets extends TableDefinition("assets"):
    val id    = Column[AssetId]("id")
    val title = Column[AssetTitle]("title")

    val * = Columns((id, title))

  object AssetEntries extends TableDefinition("asset_entries"):
    val id           = Column[EntryId]("id")
    val no           = Column[EntryNo]("no")
    val uri          = Column[EntryUri]("uri")
    val wasSeen      = Column[WasEntrySeen]("was_seen")
    val dateUploaded = Column[DateUploaded]("date_uploaded")
    val assetId      = Column[AssetId]("asset_id")

    val allExceptId = Columns((no, uri, wasSeen, dateUploaded, assetId))
    val *           = Columns((id, no, uri, wasSeen, dateUploaded, assetId))

  private val A  = Assets as "a"
  private val AE = AssetEntries as "ae"
  private val findAllColumns =
    Columns(
      A(_.id),
      A(_.title),
      AE(_.id).option,
      AE(_.no).option,
      AE(_.uri).option,
      AE(_.wasSeen).option,
      AE(_.dateUploaded).option
    )

  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): AssetRepository[F] = new:
    def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]] =
      sql"""
          SELECT ${findAllColumns} 
          FROM ${A}
          LEFT JOIN ${AE} ON ${AE(_.assetId)} = ${A(_.id)}
          ORDER BY ${A(_.id)}
      """.queryOf(findAllColumns)
        .to[List]
        .transact(xa)
        .map: rows =>
          rows
            .groupBy(row => (row._1, row._2))
            .map: (asset, records) =>
              val (assetId, assetTitle) = asset
              val entries = records
                .map: record =>
                  (
                    record._3,
                    record._4,
                    record._5,
                    record._6,
                    record._7,
                    assetId.some
                  )
                    .tupled
                    .map(Tuples.from[ExistingAssetEntry](_))
                .collect:
                  case Some(entry) => entry
              ExistingAsset(assetId, assetTitle) -> entries
            .toList
            .sortBy((asset, _) => asset.id)

    def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]] =
      exists(asset.title).flatMap:
        case true  => AddAssetError.AssetAlreadyExists.asLeft.pure
        case false => addWithoutChecking(asset).map(_.asRight)

    def delete(assetId: AssetId): F[Unit] =
      sql"DELETE FROM ${Assets} WHERE ${Assets.id} = ${assetId}"
        .update.run.transact(xa).void

    private def addWithoutChecking(asset: NewAsset): F[ExistingAsset] =
      sql"INSERT INTO ${Assets}(${Assets.title}) VALUES (${asset.title}) RETURNING ${Assets.*}"
        .queryOf(Assets.*)
        .unique
        .transact(xa)
        .map: row =>
          Tuples.from[ExistingAsset](row)

    def addEntry(entry: NewAssetEntry)
        : F[Either[AddEntryError, ExistingAssetEntry]] =
      (exists(entry.assetId), exists(entry.assetId, entry.uri)).tupled.flatMap:
        (assetExists, entryExists) =>
          if entryExists then AddEntryError.EntryAlreadyExists.asLeft.pure
          else if !assetExists then AddEntryError.AssetDoesNotExists.asLeft.pure
          else addEntryWithoutChecking(entry).map(_.asRight)

    private def addEntryWithoutChecking(entry: NewAssetEntry)
        : F[ExistingAssetEntry] =
      val values = Tuples.to(entry)
      sql"INSERT INTO ${AssetEntries}(${AssetEntries.allExceptId}) VALUES ($values) RETURNING ${AssetEntries.*}"
        .queryOf(AssetEntries.*)
        .unique
        .transact(xa)
        .map(Tuples.from[ExistingAssetEntry](_))

    private def exists(title: AssetTitle): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${Assets}
        WHERE ${Assets.title} = ${title}
      """.query[Int].option.transact(xa).map(_.isDefined)

    private def exists(id: AssetId): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${Assets}
        WHERE ${Assets.id} = ${id}
      """.query[Int].option.transact(xa).map(_.isDefined)

    private def exists(assetId: AssetId, entryUri: EntryUri): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${AssetEntries}
        WHERE ${AssetEntries.assetId} = ${assetId}
        AND ${AssetEntries.uri} = ${entryUri}
      """.query[Int].option.transact(xa).map(_.isDefined)
