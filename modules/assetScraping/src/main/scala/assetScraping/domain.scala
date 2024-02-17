package assetScraping

import java.net.URI
import cats.syntax.all.*
import core.{ Newtype, given }
import doobie.util.{ Read, Write }
import io.circe.{ Codec, Decoder, Encoder }
import library.domain.AssetId

object domain:
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
      case "Mangadex"     => Mangadex.asRight
      case "Mangakakalot" => Mangakakalot.asRight
      case other          => s"'$other' is not a valid site".asLeft
    given Encoder[Site] = Encoder[String].contramap(_.toString)

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

  enum FindScrapingConfigError:
    case AssetDoesNotExists

  enum AddScrapingConfigError:
    case ConfigAlreadyExists
    case AssetDoesNotExists
