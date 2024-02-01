package library

import java.net.URI

import core.Newtype
import core.given

object domain:

  /**
   * Asset
   */

  type AssetTitle = AssetTitle.Type
  object AssetTitle extends Newtype[String]

  case class NewAsset(title: AssetTitle)
  case class ExistingAsset(id: Long, title: AssetTitle)

  /**
   * Asset Entry
   */

  type EntryNo = EntryNo.Type
  object EntryNo extends Newtype[String]

  type EntryUri = EntryUri.Type
  object EntryUri extends Newtype[URI]

  case class NewAssetEntry(no: EntryNo, uri: EntryUri, assetId: Long)
  case class ExistingAssetEntry(id: Long, no: EntryNo, uri: EntryUri, assetId: Long)

  /**
   * Errors
   */

  enum AddAssetError:
    case AssetAlreadyExists

  enum AddEntryError:
    case EntryAlreadyExists
    case AssetDoesNotExists
