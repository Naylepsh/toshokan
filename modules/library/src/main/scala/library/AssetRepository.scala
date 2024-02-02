package library

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
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

object AssetRepository:
  object Assets extends TableDefinition("assets"):
    val id    = Column[AssetId]("id")
    val title = Column[AssetTitle]("title")

    val * = Columns((id, title))

  object AssetEntries extends TableDefinition("asset_entries"):
    val id      = Column[EntryId]("id")
    val no      = Column[EntryNo]("no")
    val uri     = Column[EntryUri]("uri")
    val assetId = Column[AssetId]("asset_id")

    val allExceptId = Columns((no, uri, assetId))
    val *           = Columns((id, no, uri, assetId))

  private val A  = Assets as "a"
  private val AE = AssetEntries as "ae"
  private val findAllColumns =
    Columns(
      A(_.id),
      A(_.title),
      AE(_.id).option,
      AE(_.no).option,
      AE(_.uri).option
    )

  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): AssetRepository[F] = new:

    def findAll: F[List[(ExistingAsset, List[ExistingAssetEntry])]] =
      sql"""
          SELECT ${findAllColumns} 
          FROM ${A}
          LEFT JOIN ${AE} ON ${AE(_.assetId)} = ${A(_.id)}
      """.queryOf(findAllColumns)
        .to[List]
        .transact(xa)
        .map: rows =>
          rows
            .groupBy(row => (row._1, row._2))
            .map: (asset, records) =>
              val (assetId, assetTitle) = asset
              val entries = records
                .map: (_, title, entryId, entryNo, entryUri) =>
                  (entryId, entryNo, entryUri, assetId.some)
                    .tupled
                    .map(Tuples.from[ExistingAssetEntry](_))
                .collect:
                  case Some(entry) => entry
              ExistingAsset(assetId, assetTitle) -> entries
            .toList

    def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]] =
      exists(asset.title).flatMap:
        case true  => AddAssetError.AssetAlreadyExists.asLeft.pure
        case false => addWithoutChecking(asset).map(_.asRight)

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
        .map: row =>
          Tuples.from[ExistingAssetEntry](row)

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
