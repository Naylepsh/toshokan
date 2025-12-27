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

case object NoMalIdAvailable extends NoStackTrace
type NoMalIdAvailable = NoMalIdAvailable.type

// TODO: Move it to category.domain?
case object CategoryDoesNotExist extends NoStackTrace
type CategoryDoesNotExist = CategoryDoesNotExist.type

case object NoTitleTranslation extends NoStackTrace
type NoTitleTranslation = NoTitleTranslation.type
