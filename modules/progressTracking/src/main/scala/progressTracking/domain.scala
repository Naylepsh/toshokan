package progressTracking
package domain

import cats.syntax.all.*
import core.Newtype
import library.category.domain.CategoryName
import library.domain.{AssetId, EntryNo}
import myAnimeList.domain.{ExternalMangaId, LatestChapter}

type MangaId = MangaId.Type
object MangaId extends Newtype[Long]:
  def apply(assetId: AssetId, categoryName: CategoryName): Option[MangaId] =
    Option.when(categoryName.toLowerCase().eqv("manga"))(MangaId(assetId.value))

type MangaTitle = MangaTitle.Type
object MangaTitle extends Newtype[String]

type MalMangaMappingId = MalMangaMappingId.Type
object MalMangaMappingId extends Newtype[Long]

case class ExistingMalMangaMapping(
    id: MalMangaMappingId,
    internalId: MangaId,
    externalId: ExternalMangaId
)

extension (no: EntryNo)
  def asLatestChapter: Option[LatestChapter] =
    no.value.toDoubleOption.map(d => LatestChapter(d.toInt))
