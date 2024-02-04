package scrapeConfigs

import java.net.URI

import core.{ Newtype, given }
import doobie.util.{ Read, Write }
import doobie.implicits.*

object domain:
  type ScrapeAssetConfigId = ScrapeAssetConfigId.Type
  object ScrapeAssetConfigId extends Newtype[Long]

  type AssetId = AssetId.Type
  object AssetId extends Newtype[Long]

  type ScrapeAssetConfigUri = ScrapeAssetConfigUri.Type
  object ScrapeAssetConfigUri extends Newtype[URI]

  type IsScrapeAssetConfigEnabled = IsScrapeAssetConfigEnabled.Type
  object IsScrapeAssetConfigEnabled extends Newtype[Boolean]

  enum Site:
    case Mangadex, Mangakakalot
  object Site:
    given Read[Site] = Read[String].map:
      case "mangadex"     => Mangadex
      case "mangakakalot" => Mangakakalot
    given Write[Site] = Write[String].contramap:
      case Mangadex     => "mangadex"
      case Mangakakalot => "mangakakalot"

  case class NewScrapeAssetConfig(
      assetId: AssetId,
      isEnabled: IsScrapeAssetConfigEnabled,
      uri: ScrapeAssetConfigUri,
      site: Site
  )

  case class ExistingScrapeAssetConfig(
      id: ScrapeAssetConfigId,
      assetId: AssetId,
      isEnabled: IsScrapeAssetConfigEnabled,
      uri: ScrapeAssetConfigUri,
      site: Site
  )

  /**
   * Errors
   */

  enum AddConfigError:
    case ConfigAlreadyExists
