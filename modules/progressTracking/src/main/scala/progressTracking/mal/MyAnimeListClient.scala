package progressTracking
package mal

import java.net.URI

import cats.effect.MonadCancelThrow
import cats.effect.std.Random
import cats.syntax.all.*
import sttp.capabilities.WebSockets
import sttp.client3.circe.*
import sttp.client3.{SttpBackend, UriContext, basicRequest}
import sttp.model.Uri

import domain.{MangaId, LatestChapter, Term}

case class MalAuth(clientId: String, clientSecret: String)

trait MyAnimeListClient[F[_]]:
  def searchManga(
      token: AuthToken,
      term: Term
  ): F[Either[Throwable, GetMangaListSuccess]]
  def updateStatus(
      token: AuthToken,
      mangaId: MangaId,
      latestChapter: LatestChapter
  ): F[Either[Throwable, Unit]]
  def refreshAuthToken(token: RefreshToken): F[Either[Throwable, AuthToken]]
  def acquireToken(
      code: String,
      codeChallenge: String
  ): F[Either[Throwable, AuthToken]]
  def generateCodeChallenge: F[String]
  def createAuthorizationLink(redirectUri: URI, codeChallenge: String): Uri

object MyAnimeListClient:
  def make[F[_]: MonadCancelThrow](
      backend: SttpBackend[F, WebSockets],
      auth: MalAuth,
      random: Random[F]
  ): MyAnimeListClient[F] = new:
    override def searchManga(
        token: AuthToken,
        term: Term
    ): F[Either[Throwable, GetMangaListSuccess]] =
      val url = uri"https://api.myanimelist.net/v2/manga?q=$term"
      basicRequest
        .get(url)
        .auth
        .bearer(token.accessToken)
        .response(asJson[GetMangaListSuccess])
        .send(backend)
        .map(_.body)
        .handleError(_.asLeft)

    override def updateStatus(
        token: AuthToken,
        mangaId: MangaId,
        latestChapter: LatestChapter
    ): F[Either[Throwable, Unit]] =
      val url =
        uri"https://api.myanimelist.net/v2/manga/$mangaId/my_list_status"
      basicRequest
        .patch(url)
        .body(UpdateMangaStatusBody(MangaStatus.Reading, latestChapter))
        .auth
        .bearer(token.accessToken)
        .send(backend)
        .map: response =>
          response.body.void.leftMap(new RuntimeException(_))

    override def refreshAuthToken(
        token: RefreshToken
    ): F[Either[Throwable, AuthToken]] =
      val url = uri"https://myanimelist.net/v1/oauth2/token"
      basicRequest
        .post(url)
        .body(
          Map(
            "client_id"     -> auth.clientId,
            "client_secret" -> auth.clientSecret,
            "grant_type"    -> "refresh_token",
            "refresh_token" -> token.value
          )
        )
        .response(asJson[AuthToken])
        .send(backend)
        .map: response =>
          response.body.leftMap(new RuntimeException(_))

    override def acquireToken(
        code: String,
        codeChallenge: String
    ): F[Either[Throwable, AuthToken]] =
      val url = uri"https://myanimelist.net/v1/oauth2/token"
      basicRequest
        .post(url)
        .body(
          Map(
            "client_id"     -> auth.clientId,
            "client_secret" -> auth.clientSecret,
            "grant_type"    -> "authorization_code",
            "code"          -> code,
            "code_verifier" -> codeChallenge
          )
        )
        .response(asJson[AuthToken])
        .send(backend)
        .map: response =>
          response.body.leftMap(new RuntimeException(_))

    override def createAuthorizationLink(
        redirectUri: URI,
        codeChallenge: String
    ): Uri =
      val params = Map(
        "client_id"             -> auth.clientId,
        "response_type"         -> "code",
        "code_challenge"        -> codeChallenge,
        "code_challenge_method" -> "plain",
        "redirect_uri"          -> redirectUri.toString
      )
      uri"https://myanimelist.net/v1/oauth2/authorize?${params}"

    override val generateCodeChallenge: F[String] =
      (1 to 50).toList
        .traverse(_ => random.nextAlphaNumeric)
        .map(_.mkString)
