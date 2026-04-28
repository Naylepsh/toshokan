package assetImporting
package domain

import java.net.URI

import util.control.NoStackTrace

type MangadexId = MangadexId.Type
object MangadexId extends neotype.Newtype[String]

type MangadexMangaUri = MangadexMangaUri.Type
object MangadexMangaUri extends neotype.Subtype[URI]:
  extension (uri: MangadexMangaUri)
    def id: MangadexId = uri.toString match
      case s"https://mangadex.org/title/$id/$_" => MangadexId(id)
      case s"https://mangadex.org/title/$id"    => MangadexId(id)

case object NoMalIdAvailable extends NoStackTrace
type NoMalIdAvailable = NoMalIdAvailable.type

case object NoTitleTranslation extends NoStackTrace
type NoTitleTranslation = NoTitleTranslation.type

enum ImportError:
  case CategoryDoesNotExist
  case AssetAlreadyExists
  case NoTitleTranslation
  case ScrapingConfigError(message: String)
  case MappingError(message: String)
