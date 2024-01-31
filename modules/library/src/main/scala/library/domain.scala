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
  case class ExistingAsset[Id](id: Id, title: AssetTitle)

  /**
   * Asset Entry
   */

  type EntryNo = EntryNo.Type
  object EntryNo extends Newtype[String]

  type EntryUri = EntryUri.Type
  object EntryUri extends Newtype[URI]

  case class NewAssetEntry(no: EntryNo, uri: EntryUri)
  case class ExistingAssetEntry[Id](id: Id, no: EntryNo, uri: EntryUri)

  /**
   * Errors
   */

  enum AddAssetError:
    case AssetAlreadyExists

  enum AddEntryError:
    case EntryAlreadyExists
    case AssetDoesNotExists
