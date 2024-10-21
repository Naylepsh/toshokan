package library
package domain

import java.net.URI
import java.time.LocalDate

import cats.kernel.Order
import cats.syntax.all.*
import core.Newtype
import core.given
import doobie.util.Read
import io.circe.{Decoder, Encoder}
import io.github.arainko.ducktape.*
import org.typelevel.cats.time.*
import util.control.NoStackTrace

import category.domain.CategoryId

/** Asset
  */

type AssetId = AssetId.Type
object AssetId extends Newtype[Long]

type AssetTitle = AssetTitle.Type
object AssetTitle extends Newtype[String]

case class NewAsset(title: AssetTitle, categoryId: Option[CategoryId])
    derives Decoder:
  def asExisting(id: AssetId): ExistingAsset =
    this.into[ExistingAsset].transform(Field.const(_.id, id))
case class ExistingAsset(
    id: AssetId,
    title: AssetTitle,
    categoryId: Option[CategoryId]
)

case object AssetAlreadyExists extends NoStackTrace
type AssetAlreadyExists = AssetAlreadyExists.type

type AddAssetError = AssetAlreadyExists

/** Asset Entry
  */

type EntryId = EntryId.Type
object EntryId extends Newtype[Long]

type EntryTitle = EntryTitle.Type
object EntryTitle extends Newtype[String]

type EntryNo = EntryNo.Type
object EntryNo extends Newtype[String]:
  given Ordering[EntryNo] with
    override def compare(x: EntryNo, y: EntryNo): Int =
      (x.value.toDoubleOption, y.value.toDoubleOption) match
        case (Some(xNo), Some(yNo)) => xNo.compareTo(yNo)
        case (Some(xNo), None)      => 1
        case (None, Some(yNo))      => -1
        case (None, None)           => 0

type EntryUri = EntryUri.Type
object EntryUri extends Newtype[URI]

type WasEntrySeen = WasEntrySeen.Type
object WasEntrySeen extends Newtype[Boolean]

type DateUploaded = DateUploaded.Type
object DateUploaded extends Newtype[LocalDate]:
  given Ordering[DateUploaded] with
    override def compare(x: DateUploaded, y: DateUploaded): Int =
      x.value.compareTo(y.value)

case class NewAssetEntry(
    title: EntryTitle,
    no: EntryNo,
    uri: EntryUri,
    dateUploaded: DateUploaded,
    assetId: AssetId
)
object NewAssetEntry:
  def make(
      title: EntryTitle,
      no: EntryNo,
      uri: EntryUri,
      dateUploaded: DateUploaded,
      assetId: AssetId
  ): NewAssetEntry =
    NewAssetEntry(title, no, uri, dateUploaded, assetId)

case class ExistingAssetEntry(
    id: EntryId,
    title: EntryTitle,
    no: EntryNo,
    uri: EntryUri,
    wasSeen: WasEntrySeen,
    dateUploaded: DateUploaded,
    assetId: AssetId
)

type Releases = (DateUploaded, List[(ExistingAsset, ExistingAssetEntry)])
object Releases:
  given Order[Releases] with
    def compare(x: Releases, y: Releases): Int =
      if x._1 == y._1 then 0
      else if x._1 < y._1 then -1
      else 1

enum AddEntryError:
  case EntryAlreadyExists
  case AssetDoesNotExists

enum UpdateEntryError:
  case AssetDoesNotExists
  case EntryDoesNotExist
