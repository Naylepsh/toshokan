package assetImporting
package domain

import core.Newt

import util.control.NoStackTrace
import core.{Newtype, given}
import java.net.URI

type MangadexId = MangadexId.Type
object MangadexId extends Newt[String]

type MangadexMangaUri = MangadexMangaUri.Type
object MangadexMangaUri extends Newtype[URI]:
  def fromString(s: URI): Either[String, MangadexMangaUri] =
    s.toString match
      case s"https://mangadex.org/title/$id/$_" => Right(MangadexMangaUri(s))
      case _ => Left(s"${s} is not a valid mangadex uri")

  extension (uri: MangadexMangaUri)
    def id: MangadexId = uri.toString match
      case s"https://mangadex.org/title/$id/$_" => MangadexId(id)

case object NoMalIdAvailable extends NoStackTrace
type NoMalIdAvailable = NoMalIdAvailable.type

// TODO: Move it to category.domain?
case object CategoryDoesNotExist extends NoStackTrace
type CategoryDoesNotExist = CategoryDoesNotExist.type
