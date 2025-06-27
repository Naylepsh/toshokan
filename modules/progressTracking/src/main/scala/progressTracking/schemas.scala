package progressTracking
package schemas

import cats.effect.Concurrent
import io.circe.Codec
import library.domain.{AssetId, WasEntrySeen}
import myAnimeList.domain.ExternalMangaId
import org.http4s.*
import org.http4s.circe.*

case class NewMalMangaMappingDTO(assetId: AssetId, malId: ExternalMangaId)
    derives Codec

given [F[_]: Concurrent]: EntityDecoder[F, NewMalMangaMappingDTO] =
  jsonOf[F, NewMalMangaMappingDTO]

case class UpdateProgressDTO(wasEntrySeen: WasEntrySeen) derives Codec

given [F[_]: Concurrent]: EntityDecoder[F, UpdateProgressDTO] =
  jsonOf[F, UpdateProgressDTO]
