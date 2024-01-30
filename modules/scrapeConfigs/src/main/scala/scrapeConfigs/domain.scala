package scrapeConfigs

import java.net.URI

import core.{ Newtype, given }

object domain:
  type SiteAssetUri = SiteAssetUri.Type
  object SiteAssetUri extends Newtype[URI]

  enum Site:
    case Mangadex, Mangakakalot

  case class SiteAsset[Id, AssetId](
      id: Id,
      assetId: AssetId,
      uri: SiteAssetUri,
      site: Site
  )
