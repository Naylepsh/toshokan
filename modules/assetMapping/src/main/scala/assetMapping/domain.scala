package assetMapping
package domain

import cats.syntax.all.*
import core.Newtype
import library.category.domain.CategoryName
import library.domain.AssetId
import myAnimeList.domain.ExternalMangaId

type MangaId = MangaId.Type
object MangaId extends Newtype[Long]:
  def apply(assetId: AssetId, categoryName: CategoryName): Option[MangaId] =
    Option.when(categoryName.toLowerCase().eqv("manga"))(MangaId(assetId.value))

type MalMangaMappingId = MalMangaMappingId.Type
object MalMangaMappingId extends Newtype[Long]

case class ExistingMalMangaMapping(
    id: MalMangaMappingId,
    internalId: MangaId,
    externalId: ExternalMangaId
)
