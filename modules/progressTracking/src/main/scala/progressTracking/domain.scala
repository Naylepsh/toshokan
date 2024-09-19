package progressTracking
package domain

import cats.syntax.all.*
import core.Newtype
import library.category.domain.CategoryName
import library.domain.AssetId
import library.domain.EntryNo

type MangaId = MangaId.Type
object MangaId extends Newtype[Long]:
  def apply(assetId: AssetId, categoryName: CategoryName): Option[MangaId] =
    Option.when(categoryName.toLowerCase().eqv("manga"))(MangaId(assetId.value))

type ExternalMangaId = ExternalMangaId.Type
object ExternalMangaId extends Newtype[Long]

type LatestChapter = LatestChapter.Type
object LatestChapter extends Newtype[Int]:
  def apply(no: EntryNo): Option[LatestChapter] =
    no.value.toDoubleOption.map(d => LatestChapter(d.toInt))

type MangaTitle = MangaTitle.Type
object MangaTitle extends Newtype[String]

type Term = Term.Type
object Term extends Newtype[String]

case class Manga(id: ExternalMangaId, title: MangaTitle)

type MalMangaMappingId = MalMangaMappingId.Type
object MalMangaMappingId extends Newtype[Long]

case class ExistingMalMangaMapping(
    id: MalMangaMappingId,
    internalId: MangaId,
    externalId: ExternalMangaId
)
