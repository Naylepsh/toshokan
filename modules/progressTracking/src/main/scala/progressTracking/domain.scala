package progressTracking
package domain

import java.time.LocalDateTime

import library.asset.domain.{EntryId, EntryNo}
import myAnimeList.domain.LatestChapter

type MangaTitle = MangaTitle.Type
object MangaTitle extends neotype.Subtype[String]

type DateMarkedSeen = DateMarkedSeen.Type
object DateMarkedSeen extends neotype.Subtype[LocalDateTime]

type WasEntrySeen = WasEntrySeen.Type
object WasEntrySeen extends neotype.Subtype[Boolean]

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
