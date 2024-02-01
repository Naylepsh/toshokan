package library

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import core.Tuples
import doobie.*
import doobie.implicits.*
import doobiex.*
import library.domain.*

trait AssetRepository[F[_]]:
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

  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): AssetRepository[F] = new:
    def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]] =
      sql"INSERT INTO ${Assets}(${Assets.title}) VALUES (${asset.title}) RETURNING ${Assets.*}"
        .queryOf(Assets.*)
        .unique
        .transact(xa)
        .map: row =>
          Tuples.from[ExistingAsset](row).asRight

    def addEntry(entry: NewAssetEntry)
        : F[Either[AddEntryError, ExistingAssetEntry]] =
      val values = Tuples.to(entry)
      sql"INSERT INTO ${AssetEntries}(${AssetEntries.allExceptId}) VALUES ($values) RETURNING ${AssetEntries.*}"
        .queryOf(AssetEntries.*)
        .unique
        .transact(xa)
        .map: row =>
          Tuples.from[ExistingAssetEntry](row).asRight
