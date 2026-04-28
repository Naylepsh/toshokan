package assetMapping
package domain

import library.category.domain.MangaId
import myAnimeList.domain.ExternalMangaId

type MalMangaMappingId = MalMangaMappingId.Type
object MalMangaMappingId extends neotype.Newtype[Long]

case class ExistingMalMangaMapping(
    id: MalMangaMappingId,
    internalId: MangaId,
    externalId: ExternalMangaId
)
