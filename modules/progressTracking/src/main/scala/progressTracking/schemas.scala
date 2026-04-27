package progressTracking
package schemas

import cats.effect.IO
import io.circe.Codec
import library.asset.domain.AssetId
import myAnimeList.domain.ExternalMangaId
import neotype.interop.circe.given
import org.http4s.*
import org.http4s.circe.*

import domain.WasEntrySeen

case class NewMalMangaMappingDTO(assetId: AssetId, malId: ExternalMangaId)
    derives Codec

given EntityDecoder[IO, NewMalMangaMappingDTO] =
  jsonOf[IO, NewMalMangaMappingDTO]

case class UpdateProgressDTO(wasEntrySeen: WasEntrySeen) derives Codec

given EntityDecoder[IO, UpdateProgressDTO] =
  jsonOf[IO, UpdateProgressDTO]
