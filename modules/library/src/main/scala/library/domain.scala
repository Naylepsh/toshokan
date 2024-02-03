package library

import java.net.URI

import core.Newtype
import core.given
import org.typelevel.cats.time.*
import java.time.LocalDate
import doobie.implicits.javatimedrivernative.*

object domain:

  /**
   * Asset
   */

  type AssetId = AssetId.Type
  object AssetId extends Newtype[Long]

  type AssetTitle = AssetTitle.Type
  object AssetTitle extends Newtype[String]

  case class NewAsset(title: AssetTitle)
  case class ExistingAsset(id: AssetId, title: AssetTitle)

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

  /**
   * Errors
   */

  enum AddAssetError:
    case AssetAlreadyExists

  enum AddEntryError:
    case EntryAlreadyExists
    case AssetDoesNotExists
