package progressTracking
package domain

import java.time.LocalDateTime

import cats.syntax.all.*
import library.category.domain.CategoryName
import library.domain.{AssetId, EntryId, EntryNo}
import myAnimeList.domain.{ExternalMangaId, LatestChapter}
import neotype.*

type MangaId = MangaId.Type
object MangaId extends neotype.Newtype[Long]:
  def apply(assetId: AssetId, categoryName: CategoryName): Option[MangaId] =
    Option
      .when(categoryName.toLowerCase().eqv("manga"))(MangaId(assetId.unwrap))

type MangaTitle = MangaTitle.Type
object MangaTitle extends neotype.Subtype[String]

type MalMangaMappingId = MalMangaMappingId.Type
object MalMangaMappingId extends neotype.Newtype[Long]

type DateMarkedSeen = DateMarkedSeen.Type
object DateMarkedSeen extends neotype.Subtype[LocalDateTime]

type WasEntrySeen = WasEntrySeen.Type
object WasEntrySeen extends neotype.Subtype[Boolean]

case class ExistingMalMangaMapping(
    id: MalMangaMappingId,
    internalId: MangaId,
    externalId: ExternalMangaId
)

case class EntryProgress(
    entryId: EntryId,
    wasSeen: WasEntrySeen,
    dateMarkedSeen: Option[DateMarkedSeen]
)

object EntryProgress:
  def notSeen(entryId: EntryId): EntryProgress =
    EntryProgress(entryId, WasEntrySeen(false), None)

extension (no: EntryNo)
  def asLatestChapter: Option[LatestChapter] =
    no.toDoubleOption.map(d => LatestChapter(d.toInt))
