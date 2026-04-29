package assetScraping.configs.domain

import java.net.URI

import scala.util.chaining.*

import cats.syntax.all.*
import doobie.util.{Read, Write}
import io.circe.{Decoder, Encoder}
import library.asset.domain.AssetId
import library.author.domain.AuthorId

type AssetScrapingConfigId = AssetScrapingConfigId.Type
object AssetScrapingConfigId extends neotype.Newtype[Long]

type ScrapingConfigUri = ScrapingConfigUri.Type
object ScrapingConfigUri extends neotype.Subtype[URI]

type IsConfigEnabled = IsConfigEnabled.Type
object IsConfigEnabled extends neotype.Subtype[Boolean]:
  given Decoder[IsConfigEnabled] =
    Decoder[Boolean].map(IsConfigEnabled.apply) or Decoder[String].emap:
      case "true" | "on" => IsConfigEnabled(true).asRight
      case "false"       => IsConfigEnabled(false).asRight
      case other =>
        s"""${other} is not one of [true, false, "true", "false", "on"]""".asLeft

enum AssetSite:
  case Mangakakalot, Mangadex, Yatta, Empik, DynastyScans, Batoto

object AssetSite:
  // SQL
  given Read[AssetSite] = Read[String].map:
    case "mangadex"      => Mangadex
    case "mangakakalot"  => Mangakakalot
    case "yatta"         => Yatta
    case "empik"         => Empik
    case "dynasty-scans" => DynastyScans
    case "batoto"        => Batoto
  given Write[AssetSite] = Write[String].contramap:
    case Mangadex     => "mangadex"
    case Mangakakalot => "mangakakalot"
    case Yatta        => "yatta"
    case Empik        => "empik"
    case DynastyScans => "dynasty-scans"
    case Batoto       => "batoto"

  // JSON
  given Decoder[AssetSite] = Decoder[String].emap:
    case "Mangadex"     => Mangadex.asRight
    case "Mangakakalot" => Mangakakalot.asRight
    case "Yatta"        => Yatta.asRight
    case "Empik"        => Empik.asRight
    case "DynastyScans" => DynastyScans.asRight
    case "Batoto"       => Batoto.asRight
    case other          => s"'$other' is not a valid asset site".asLeft
  given Encoder[AssetSite] = Encoder[String].contramap(_.toString)

case class NewAssetScrapingConfig private (
    uri: ScrapingConfigUri,
    site: AssetSite,
    isEnabled: IsConfigEnabled,
    assetId: AssetId
)

object NewAssetScrapingConfig:
  private case class SiteUriRule(
      pattern: scala.util.matching.Regex,
      normalize: (
          ScrapingConfigUri,
          scala.util.matching.Regex.Match
      ) => ScrapingConfigUri = (uri, _) => uri
  )

  private val siteRules: Map[AssetSite, SiteUriRule] = Map(
    AssetSite.Mangadex -> SiteUriRule(
      "^https://mangadex.org/title/([-a-zA-z0-9]+)(?:/.*)?$".r,
      (_, m) =>
        ScrapingConfigUri(URI(s"https://mangadex.org/title/${m.group(1)}"))
    ),
    AssetSite.Mangakakalot -> SiteUriRule(
      "^https://(mangakakalot.com|chapmanganato.to|chapmanganato.com|www.mangakakalot.gg)/.+".r
    ),
    AssetSite.Yatta -> SiteUriRule("^https://yatta.pl/.+".r),
    AssetSite.Empik -> SiteUriRule("^https://www.empik.com/ksiazki.+".r),
    AssetSite.DynastyScans -> SiteUriRule(
      "^https://dynasty-scans.com/series/.+".r
    ),
    AssetSite.Batoto -> SiteUriRule(
      "^https://bato.si/title/([0-9]+)(?:-.*)?/?.*".r,
      (_, m) => ScrapingConfigUri(URI(s"https://bato.si/title/${m.group(1)}"))
    )
  )

  def apply(
      uri: ScrapingConfigUri,
      site: AssetSite,
      isEnabled: IsConfigEnabled,
      assetId: AssetId
  ): Either[String, NewAssetScrapingConfig] =
    normalize(uri, site, isEnabled, assetId).map:
      (uri, site, isEnabled, assetId) =>
        new NewAssetScrapingConfig(uri, site, isEnabled, assetId)

  def normalize(
      uri: ScrapingConfigUri,
      site: AssetSite,
      isEnabled: IsConfigEnabled,
      assetId: AssetId
  ) =
    normalizeUri(uri, site).map: uri =>
      (uri, site, isEnabled, assetId)

  private def normalizeUri(
      uri: ScrapingConfigUri,
      site: AssetSite
  ): Either[String, ScrapingConfigUri] =
    siteRules(site).pipe: rule =>
      rule.pattern
        .findFirstMatchIn(uri.toString)
        .map(rule.normalize(uri, _))
        .toRight(s"Uri: $uri is not a valid config uri of site: $site")

case class ExistingAssetScrapingConfig(
    id: AssetScrapingConfigId,
    uri: ScrapingConfigUri,
    site: AssetSite,
    isEnabled: IsConfigEnabled,
    assetId: AssetId
)
object ExistingAssetScrapingConfig:
  def apply(
      id: AssetScrapingConfigId,
      uri: ScrapingConfigUri,
      site: AssetSite,
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

type AuthorScrapingConfigId = AuthorScrapingConfigId.Type
object AuthorScrapingConfigId extends neotype.Newtype[Long]

enum AuthorSite:
  case Hitomi

object AuthorSite:
  // SQL
  given Read[AuthorSite] = Read[String].map:
    case "hitomi" => Hitomi
  given Write[AuthorSite] = Write[String].contramap:
    case Hitomi => "hitomi"

  // JSON
  given Decoder[AuthorSite] = Decoder[String].emap:
    case "Hitomi" => Hitomi.asRight
    case other    => s"'$other' is not a valid site".asLeft
  given Encoder[AuthorSite] = Encoder[String].contramap(_.toString)

case class NewAuthorScrapingConfig private (
    uri: ScrapingConfigUri,
    site: AuthorSite,
    isEnabled: IsConfigEnabled,
    authorId: AuthorId
)

object NewAuthorScrapingConfig:
  private val hitomiUri = "^https://hitomi.la/artist/(.+).html$".r

  def make(
      uri: ScrapingConfigUri,
      site: AuthorSite,
      isEnabled: IsConfigEnabled,
      authorId: AuthorId
  ): Either[String, NewAuthorScrapingConfig] =
    normalizeUri(uri, site).map: uri =>
      NewAuthorScrapingConfig(uri, site, isEnabled, authorId)

  def normalizeUri(
      uri: ScrapingConfigUri,
      site: AuthorSite
  ): Either[String, ScrapingConfigUri] =
    site match
      case AuthorSite.Hitomi =>
        uri.toString match
          case hitomiUri(_) => uri.asRight
          case other =>
            s"Uri: $other is not a valid config uri of site: $site".asLeft

case class ExistingAuthorScrapingConfig private (
    id: AuthorScrapingConfigId,
    uri: ScrapingConfigUri,
    site: AuthorSite,
    isEnabled: IsConfigEnabled,
    authorId: AuthorId
)

enum AddAuthorScrapingConfig:
  case ConfigAlreadyExists, AuthorDoesNotExist
