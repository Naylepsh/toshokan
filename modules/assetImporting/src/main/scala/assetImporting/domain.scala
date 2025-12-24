package assetImporting
package domain

import java.net.URI

import core.given
import core.{Newt, Newtype}

import util.control.NoStackTrace

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

case object NoTitleTranslation extends NoStackTrace
type NoTitleTranslation = NoTitleTranslation.type
