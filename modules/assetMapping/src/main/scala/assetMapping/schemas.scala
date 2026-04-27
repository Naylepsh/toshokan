package assetMapping
package schemas

import cats.effect.IO
import io.circe.Codec
import library.asset.domain.AssetId
import myAnimeList.domain.ExternalMangaId
import neotype.interop.circe.given
import org.http4s.*
import org.http4s.circe.*

case class NewMalMangaMappingDTO(assetId: AssetId, malId: ExternalMangaId)
    derives Codec

given EntityDecoder[IO, NewMalMangaMappingDTO] =
  jsonOf[IO, NewMalMangaMappingDTO]
