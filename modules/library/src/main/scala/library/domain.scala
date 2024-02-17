package library

import java.net.URI
import java.time.LocalDate

import cats.syntax.all.*
import core.Newtype
import core.given
import doobie.implicits.legacy.localdate.*
import doobie.util.{ Read, Write }
import io.circe.{ Codec, Decoder, Encoder }
import io.github.arainko.ducktape.*
import org.typelevel.cats.time.*
import cats.kernel.Order

object domain:

  /**
   * Asset
   */

  type AssetId = AssetId.Type
  object AssetId extends Newtype[Long]

  type AssetTitle = AssetTitle.Type
  object AssetTitle extends Newtype[String]

  case class NewAsset(title: AssetTitle) derives Decoder:
    def asExisting(id: AssetId): ExistingAsset =
      this.into[ExistingAsset].transform(Field.const(_.id, id))
  case class ExistingAsset(id: AssetId, title: AssetTitle)

  enum AddAssetError:
    case AssetAlreadyExists

  /**
   * Asset Entry
   */

  type EntryId = EntryId.Type
  object EntryId extends Newtype[Long]

  type EntryNo = EntryNo.Type
  object EntryNo extends Newtype[String]

  type EntryUri = EntryUri.Type
  object EntryUri extends Newtype[URI]

  type WasEntrySeen = WasEntrySeen.Type
  object WasEntrySeen extends Newtype[Boolean]

  type DateUploaded = DateUploaded.Type
  object DateUploaded extends Newtype[LocalDate]

  case class NewAssetEntry(
      no: EntryNo,
      uri: EntryUri,
      wasSeen: WasEntrySeen,
      dateUploaded: DateUploaded,
      assetId: AssetId
  )
  case class ExistingAssetEntry(
      id: EntryId,
      no: EntryNo,
      uri: EntryUri,
      wasSeen: WasEntrySeen,
      dateUploaded: DateUploaded,
      assetId: AssetId
  )

  type Releases = (DateUploaded, List[(ExistingAsset, ExistingAssetEntry)])
  object Releases:
    given Order[Releases] with
      def compare(x: Releases, y: Releases): Int =
        if x._1 == y._1 then 0
        else if x._1 < y._1 then -1
        else 1

  enum AddEntryError:
    case EntryAlreadyExists
    case AssetDoesNotExists

