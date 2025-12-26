package progressTracking
package domain

import cats.syntax.all.*
import library.category.domain.CategoryName
import library.domain.{AssetId, EntryNo}
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

case class ExistingMalMangaMapping(
    id: MalMangaMappingId,
    internalId: MangaId,
    externalId: ExternalMangaId
)

extension (no: EntryNo)
  def asLatestChapter: Option[LatestChapter] =
    no.toDoubleOption.map(d => LatestChapter(d.toInt))
