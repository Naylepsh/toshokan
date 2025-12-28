package library
package domain

import java.net.URI
import java.time.LocalDate

import cats.effect.kernel.Sync
import cats.kernel.Order
import cats.syntax.all.*
import io.circe.Decoder
import io.github.arainko.ducktape.*
import neotype.interop.circe.given
import org.typelevel.cats.time.*

import util.control.NoStackTrace
import category.domain.CategoryId

/** Asset
  */

type AssetId = AssetId.Type
object AssetId extends neotype.Newtype[Long]

type AssetTitle = AssetTitle.Type
object AssetTitle extends neotype.Subtype[String]

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

type DaysSinceRelease = DaysSinceRelease.Type
object DaysSinceRelease extends neotype.Subtype[Long]

case class StaleAsset(
    asset: ExistingAsset,
    lastRelease: DateUploaded,
    daysSinceLastRelease: DaysSinceRelease
)

/** Asset Entry
  */

type EntryId = EntryId.Type
object EntryId extends neotype.Newtype[Long]

type EntryTitle = EntryTitle.Type
object EntryTitle extends neotype.Subtype[String]

type EntryNo = EntryNo.Type
object EntryNo extends neotype.Subtype[String]:
  given Ordering[EntryNo] with
    override def compare(x: EntryNo, y: EntryNo): Int =
      (x.toDoubleOption, y.toDoubleOption) match
        case (Some(xNo), Some(yNo)) => xNo.compareTo(yNo)
        case (Some(xNo), None)      => 1
        case (None, Some(yNo))      => -1
        case (None, None)           => 0

type EntryUri = EntryUri.Type
object EntryUri extends neotype.Subtype[URI]

type DateUploaded = DateUploaded.Type
object DateUploaded extends neotype.Subtype[LocalDate]:
  extension (self: DateUploaded)
    def daysAgo[F[_]: Sync]: F[DaysSinceRelease] =
      Sync[F]
        .delay:
          java.time.temporal.ChronoUnit.DAYS
            .between(self, java.time.LocalDate.now())
        .map(DaysSinceRelease(_))

  given Ordering[DateUploaded] with
    override def compare(x: DateUploaded, y: DateUploaded): Int =
      x.compareTo(y)

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
    dateUploaded: DateUploaded,
    assetId: AssetId
)

type Releases = (DateUploaded, List[(ExistingAsset, ExistingAssetEntry)])
object Releases:
  given Order[Releases] with
    def compare(x: Releases, y: Releases): Int =
      val xdt: LocalDate = x._1
      val ydt: LocalDate = y._1
      if xdt == ydt then 0
      else if xdt < ydt then -1
      else 1

enum AddEntryError:
  case EntryAlreadyExists
  case AssetDoesNotExists

enum UpdateEntryError:
  case AssetDoesNotExists
  case EntryDoesNotExist
