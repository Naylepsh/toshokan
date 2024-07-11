package progressTracking.mal

import io.circe.derivation.{Configuration, ConfiguredDecoder, ConfiguredEncoder}
import io.circe.{Decoder, Encoder}

given Configuration = Configuration.default.withSnakeCaseMemberNames

case class Manga(id: Long, title: String) derives ConfiguredDecoder

case class GetMangaListData(node: Manga) derives ConfiguredDecoder

case class GetMangaListSuccess(data: List[GetMangaListData])
    derives ConfiguredDecoder

enum MangaStatus:
  case Reading, Completed, OnHold, Dropped, PlanToRead
object MangaStatus:
  given Encoder[MangaStatus] = Encoder.AsObject.derivedConfigured

case class UpdateMangaStatusBody(status: MangaStatus, numChaptersRead: Int)
    derives ConfiguredEncoder

case class AuthToken(expiresIn: Long, refreshToken: String, accessToken: String)
    derives ConfiguredDecoder
