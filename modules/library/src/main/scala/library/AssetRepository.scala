package library

import cats.syntax.all.*
import cats.effect.MonadCancelThrow
import library.domain.*
import doobie.*
import doobie.implicits.*
import doobiex.*
import core.Tuples

trait AssetRepository[F[_]]:
  def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]]
  def addEntries(
      assetId: Long,
      entry: NewAssetEntry
  ): F[Either[AddEntryError, ExistingAssetEntry]]

object AssetRepository:
  object Assets extends TableDefinition("assets"):
    val id    = Column[Long]("id")
    val title = Column[AssetTitle]("title")

    val * = Columns((id, title))

  object AssetEntries extends TableDefinition("asset_entries"):
    val id  = Column[Long]("id")
    val no  = Column[EntryNo]("no")
    val uri = Column[EntryUri]("uri")

    val * = Columns((id, no, uri))

  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): AssetRepository[F] = new:
    def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset]] =
      sql"INSERT INTO ${Assets}(${Assets.title}) VALUES (${asset.title}) RETURNING ${Assets.*}"
        .queryOf(Assets.*)
        .unique
        .transact(xa)
        .map: row =>
          Tuples.from[ExistingAsset](row).asRight

    def addEntries(
        assetId: Long,
        entry: NewAssetEntry
    ): F[Either[AddEntryError, ExistingAssetEntry]] = ???
