package library

import java.net.URI

import core.Newtype
import core.given

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

  case class NewAssetEntry(no: EntryNo, uri: EntryUri, assetId: AssetId)
  case class ExistingAssetEntry(id: EntryId, no: EntryNo, uri: EntryUri, assetId: AssetId)

  /**
   * Errors
   */

  enum AddAssetError:
    case AssetAlreadyExists

  enum AddEntryError:
    case EntryAlreadyExists
    case AssetDoesNotExists
