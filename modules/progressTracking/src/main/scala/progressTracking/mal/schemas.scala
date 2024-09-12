package progressTracking.mal

import core.Newtype
import io.circe.derivation.{Configuration, ConfiguredDecoder}
import io.circe.{Decoder, Encoder}

given Configuration = Configuration.default.withSnakeCaseMemberNames

case class Manga(id: Long, title: String) derives ConfiguredDecoder

case class GetMangaListData(node: Manga) derives ConfiguredDecoder

case class GetMangaListSuccess(data: List[GetMangaListData])
    derives ConfiguredDecoder

enum MangaStatus:
  case Reading, Completed, OnHold, Dropped, PlanToRead

  val urlEncoded: String = Configuration.default.withSnakeCaseConstructorNames
    .transformConstructorNames(this.toString)

type RefreshToken = RefreshToken.Type
object RefreshToken extends Newtype[String]

type AccessToken = AccessToken.Type
object AccessToken extends Newtype[String]

case class AuthToken(
    expiresIn: Long,
    refreshToken: RefreshToken,
    accessToken: AccessToken
) derives ConfiguredDecoder
