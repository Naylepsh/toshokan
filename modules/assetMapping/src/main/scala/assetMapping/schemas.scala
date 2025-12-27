package assetMapping
package schemas

import cats.effect.Concurrent
import io.circe.Codec
import library.domain.AssetId
import myAnimeList.domain.ExternalMangaId
import org.http4s.*
import org.http4s.circe.*
import neotype.interop.circe.given

case class NewMalMangaMappingDTO(assetId: AssetId, malId: ExternalMangaId)
    derives Codec

given [F[_]: Concurrent]: EntityDecoder[F, NewMalMangaMappingDTO] =
  jsonOf[F, NewMalMangaMappingDTO]
