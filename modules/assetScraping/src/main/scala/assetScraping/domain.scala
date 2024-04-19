package assetScraping
package domain

import java.net.URI

import cats.syntax.all.*
import core.{ Newtype, given }
import doobie.util.{ Read, Write }
import io.circe.{ Decoder, Encoder }
import library.domain.AssetId

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
)

object NewAssetScrapingConfig:
  private val mangadexUriWithTitleRegex =
    "^https://mangadex.org/title/([-a-zA-z0-9]+)/(.+)$".r
  private val mangadexUriWithoutTitleRegex =
    "^https://mangadex.org/title/([-a-zA-z0-9]+)/?$".r
  /*
   * There are multiple formats in mangakakalot site family
   * and including them all is a PITA.
   * Hence a simple domain check will do
   */
  private val mangakakalotUri =
    "^https://(mangakakalot.com|chapmanganato.to|chapmanganato.com)/.+".r

  def apply(
      uri: ScrapingConfigUri,
      site: Site,
      isEnabled: IsConfigEnabled,
      assetId: AssetId
  ): Either[String, NewAssetScrapingConfig] =
    val normalizedUri = site match
      case Site.Mangadex =>
        uri.value.toString match
          case mangadexUriWithTitleRegex(_, title) =>
            ScrapingConfigUri(
              URI(uri.value.toString.replace(s"/$title", ""))
            ).asRight
          case mangadexUriWithoutTitleRegex(mangaId) =>
            ScrapingConfigUri(
              URI(s"https://mangadex.org/title/$mangaId")
            ).asRight
          case other =>
            s"Uri: $other is not a valid config uri of site: $site".asLeft
      case Site.Mangakakalot =>
        uri.value.toString match
          case mangakakalotUri(_) => uri.asRight
          case other =>
            s"Uri: $other is not a valid config uri of site: $site".asLeft
    normalizedUri.map: uri =>
      new NewAssetScrapingConfig(uri, site, isEnabled, assetId)

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

case class ScrapingSummary(
    newEntriesCount: Int,
    errorsCount: Int,
    scrapingTimeSeconds: Long
)
