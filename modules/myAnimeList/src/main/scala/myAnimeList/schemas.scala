package myAnimeList
package schemas

import io.circe.derivation.{Configuration, ConfiguredDecoder}
import io.circe.{Decoder}
import neotype.interop.circe.given

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
object RefreshToken extends neotype.Subtype[String]

type AccessToken = AccessToken.Type
object AccessToken extends neotype.Subtype[String]

case class AuthToken(
    expiresIn: Long,
    refreshToken: RefreshToken,
    accessToken: AccessToken
) derives ConfiguredDecoder
