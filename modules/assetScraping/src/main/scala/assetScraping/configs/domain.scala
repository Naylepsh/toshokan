package assetScraping.configs.domain

import java.net.URI

import cats.syntax.all.*
import core.{Newtype, given}
import doobie.util.{Read, Write}
import io.circe.{Decoder, Encoder}
import library.domain.AssetId

type AssetScrapingConfigId = AssetScrapingConfigId.Type
object AssetScrapingConfigId extends Newtype[Long]

type ScrapingConfigUri = ScrapingConfigUri.Type
object ScrapingConfigUri extends Newtype[URI]

type IsConfigEnabled = IsConfigEnabled.Type
object IsConfigEnabled extends Newtype[Boolean]

enum Site:
  case Mangakakalot, Mangadex, Yatta, Hitomi, Empik, DynastyScans, Batoto

object Site:
  // SQL
  given Read[Site] = Read[String].map:
    case "mangadex"      => Mangadex
    case "mangakakalot"  => Mangakakalot
    case "yatta"         => Yatta
    case "hitomi"        => Hitomi
    case "empik"         => Empik
    case "dynasty-scans" => DynastyScans
    case "batoto"        => Batoto
  given Write[Site] = Write[String].contramap:
    case Mangadex     => "mangadex"
    case Mangakakalot => "mangakakalot"
    case Yatta        => "yatta"
    case Hitomi       => "hitomi"
    case Empik        => "empik"
    case DynastyScans => "dynasty-scans"
    case Batoto       => "batoto"

  // JSON
  given Decoder[Site] = Decoder[String].emap:
    case "Mangadex"     => Mangadex.asRight
    case "Mangakakalot" => Mangakakalot.asRight
    case "Yatta"        => Yatta.asRight
    case "Hitomi"       => Hitomi.asRight
    case "Empik"        => Empik.asRight
    case "DynastyScans" => DynastyScans.asRight
    case "Batoto"       => Batoto.asRight
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
    "^https://mangadex.org/title/([-a-zA-z0-9]+)/(.+)/?$".r
  private val mangadexUriWithoutTitleRegex =
    "^https://mangadex.org/title/([-a-zA-z0-9]+)/?$".r
  /*
   * There are multiple formats in mangakakalot site family
   * and including them all is a PITA.
   * Hence a simple domain check will do
   */
  private val mangakakalotUri =
    "^https://(mangakakalot.com|chapmanganato.to|chapmanganato.com|www.mangakakalot.gg)/.+".r
  private val yattaUri        = "^https://yatta.pl/.+".r
  private val hitomiUri       = "^https://hitomi.la/artist/(.+).html$".r
  private val empikUri        = "^https://www.empik.com/ksiazki.+".r
  private val dynastyScansUri = "^https://dynasty-scans.com/series/.+".r
  private val batotoUri = "^https://(bato.to|xbato.com)/series/([0-9]+)/?.*".r

  def apply(
      uri: ScrapingConfigUri,
      site: Site,
      isEnabled: IsConfigEnabled,
      assetId: AssetId
  ): Either[String, NewAssetScrapingConfig] =
    normalize(uri, site, isEnabled, assetId).map:
      (uri, site, isEnabled, assetId) =>
        new NewAssetScrapingConfig(uri, site, isEnabled, assetId)

  def normalize(
      uri: ScrapingConfigUri,
      site: Site,
      isEnabled: IsConfigEnabled,
      assetId: AssetId
  ) =
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
      case Site.Yatta =>
        uri.value.toString match
          case yattaUri() => uri.asRight
          case other =>
            s"Uri: $other is not a valid config uri of site: $site".asLeft
      case Site.Hitomi =>
        uri.value.toString match
          case hitomiUri(_) => uri.asRight
          case other =>
            s"Uri: $other is not a valid config uri of site: $site".asLeft
      case Site.Empik =>
        uri.value.toString match
          case empikUri() => uri.asRight
          case other =>
            s"Uri: $other is not a valid config uri of site: $site".asLeft
      case Site.DynastyScans =>
        uri.value.toString match
          case dynastyScansUri() => uri.asRight
          case other =>
            s"Uri: $other is not a valid config uri of site: $site".asLeft
      case Site.Batoto =>
        uri.value.toString match
          case batotoUri(domain, mangaId) =>
            ScrapingConfigUri(URI(s"https://${domain}/series/$mangaId")).asRight
          case other =>
            s"Uri: $other is not a valid config uri of site: $site".asLeft

    normalizedUri.map: uri =>
      (uri, site, isEnabled, assetId)

case class ExistingAssetScrapingConfig(
    id: AssetScrapingConfigId,
    uri: ScrapingConfigUri,
    site: Site,
    isEnabled: IsConfigEnabled,
    assetId: AssetId
)
object ExistingAssetScrapingConfig:
  def apply(
      id: AssetScrapingConfigId,
      uri: ScrapingConfigUri,
      site: Site,
      isEnabled: IsConfigEnabled,
      assetId: AssetId
  ): Either[String, ExistingAssetScrapingConfig] =
    NewAssetScrapingConfig
      .normalize(uri, site, isEnabled, assetId)
      .map: (uri, site, isEnabled, assetId) =>
        new ExistingAssetScrapingConfig(
          id,
          uri,
          site,
          isEnabled,
          assetId
        )

enum FindScrapingConfigError:
  case AssetDoesNotExists

enum AddScrapingConfigError:
  case ConfigAlreadyExists
  case AssetDoesNotExists

enum UpdateScrapingConfigError:
  case AssetDoesNotExists
  case ConfigDoesNotExist
  case ConflictingConfigError
