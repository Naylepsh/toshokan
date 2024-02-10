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

  enum AddEntryError:
    case EntryAlreadyExists
    case AssetDoesNotExists

  /**
   * Asset Scraping Config
   */

  type AssetScrapingConfigId = AssetScrapingConfigId.Type
  object AssetScrapingConfigId extends Newtype[Long]

  type ScrapingConfigUri = ScrapingConfigUri.Type
  object ScrapingConfigUri extends Newtype[URI]

  type IsConfigEnabled = IsConfigEnabled.Type
  object IsConfigEnabled extends Newtype[Boolean]

  enum Site:
    case Mangakakalot, Mangadex
  object Site:
    given Read[Site] = Read[String].map:
      case "mangadex"     => Mangadex
      case "mangakakalot" => Mangakakalot
    given Write[Site] = Write[String].contramap:
      case Mangadex     => "mangadex"
      case Mangakakalot => "mangakakalot"
    given Decoder[Site] = Decoder[String].emap:
      case "mangadex"     => Mangadex.asRight
      case "mangakakalot" => Mangakakalot.asRight
      case other          => s"'$other' is not a valid site".asLeft
    given Encoder[Site] = Encoder[String].contramap:
      case Mangadex     => "mangadex"
      case Mangakakalot => "mangakakalot"

  case class NewAssetScrapingConfig(
      uri: ScrapingConfigUri,
      site: Site,
      isEnabled: IsConfigEnabled,
      assetId: AssetId
  ) derives Decoder
  case class ExistingAssetScrapingConfig(
      id: AssetScrapingConfigId,
      uri: ScrapingConfigUri,
      site: Site,
      isEnabled: IsConfigEnabled,
      assetId: AssetId
  )

  enum AddScrapingConfigError:
    case ConfigAlreadyExists
    case AssetDoesNotExists
