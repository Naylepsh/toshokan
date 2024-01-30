package library

import java.util.UUID
import javax.sql.DataSource

import scala.reflect.ClassTag

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.all.*
import com.augustnagro.magnum.*
import library.domain.*

trait AssetRepository[F[_], AssetId, AssetEntryId]:
  def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset[AssetId]]]
  def addEntries(
      asset: AssetId,
      entries: NonEmptyList[NewAssetEntry]
  ): F[Either[AddEntryError, ExistingAssetEntry[AssetEntryId]]]

object AssetRepository:
  case class AssetCreator(
      title: AssetTitle
  )
  object AssetCreator:
    def of(asset: NewAsset): AssetCreator = AssetCreator(asset.title)

  @Table(SqliteDbType)
  case class Asset[AssetId](
      @Id id: AssetId,
      title: AssetTitle
  ) derives DbCodec:
    def toDomain: ExistingAsset[AssetId] = ExistingAsset(id, title)

  def make[
      F[_]: Applicative,
      AssetId: ClassTag: DbCodec,
      AssetEntryId: ClassTag: DbCodec
  ](ds: DataSource): AssetRepository[F, AssetId, AssetEntryId] = new:
    val repo = Repo[AssetCreator, Asset[AssetId], AssetId]

    def add(asset: NewAsset)
        : F[Either[AddAssetError, ExistingAsset[AssetId]]] =
      val spec = Spec[Asset[AssetId]]
        .where(sql"title = ${asset.title}")
        .limit(1)

      Applicative[F].pure:
        connect(ds):
          val assetAlreadyExists = repo.findAll(spec).length > 0
          if assetAlreadyExists
          then AddAssetError.AssetAlreadyExists.asLeft
          else
            repo.insert(AssetCreator.of(asset))
            repo.findAll(spec).head.toDomain.asRight

    override def addEntries(
        asset: AssetId,
        entries: NonEmptyList[NewAssetEntry]
    ): F[Either[AddEntryError, ExistingAssetEntry[AssetEntryId]]] = ???
