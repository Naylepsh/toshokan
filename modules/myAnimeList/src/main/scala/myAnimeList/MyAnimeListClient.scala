package myAnimeList

import java.net.URI

import cats.effect.MonadCancelThrow
import cats.effect.std.Random
import cats.syntax.all.*
import sttp.capabilities.WebSockets
import sttp.client3.circe.*
import sttp.client3.{SttpBackend, UriContext, basicRequest}
import sttp.model.{StatusCode, Uri}

import domain.{LatestChapter, Term, ExternalMangaId}
import schemas.*

case class MalAuth(clientId: String, clientSecret: String, redirectUri: URI)

trait MyAnimeListClient[F[_]]:
  def searchManga(
      token: AuthToken,
      term: Term.Name
  ): F[Either[Throwable, GetMangaListSuccess]]
  def find(
      token: AuthToken,
      mangaId: ExternalMangaId
  ): F[Either[Throwable, Option[Manga]]]
  def updateStatus(
      token: AuthToken,
      mangaId: ExternalMangaId,
      latestChapter: LatestChapter
  ): F[Either[Throwable, Unit]]
  def refreshAuthToken(token: RefreshToken): F[Either[Throwable, AuthToken]]
  def acquireToken(
      code: String,
      codeChallenge: String
  ): F[Either[Throwable, AuthToken]]
  def generateCodeChallenge: F[String]
  def createAuthorizationLink(codeChallenge: String): Uri

object MyAnimeListClient:
  def make[F[_]: MonadCancelThrow](
      backend: SttpBackend[F, WebSockets],
      auth: MalAuth,
      random: Random[F]
  ): MyAnimeListClient[F] = new:
    override def searchManga(
        token: AuthToken,
        term: Term.Name
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

    override def find(
        token: AuthToken,
        mangaId: ExternalMangaId
    ): F[Either[Throwable, Option[Manga]]] =
      val url = uri"https://api.myanimelist.net/v2/manga/$mangaId"
      basicRequest
        .get(url)
        .auth
        .bearer(token.accessToken)
        .response(asJson[Manga])
        .send(backend)
        .map: response =>
          response.code match
            case StatusCode.NotFound => None.asRight
            case _ => response.body.map(_.some).leftMap(new RuntimeException(_))

    override def updateStatus(
        token: AuthToken,
        mangaId: ExternalMangaId,
        latestChapter: LatestChapter
    ): F[Either[Throwable, Unit]] =
      val url =
        uri"https://api.myanimelist.net/v2/manga/$mangaId/my_list_status"
      basicRequest
        .patch(url)
        .body(
          "status"            -> MangaStatus.Reading.urlEncoded,
          "num_chapters_read" -> latestChapter.value.toString
        )
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
            "code_verifier" -> codeChallenge,
            "redirect_uri"  -> auth.redirectUri.toString
          )
        )
        .response(asJson[AuthToken])
        .send(backend)
        .map: response =>
          response.body.leftMap(new RuntimeException(_))

    override def createAuthorizationLink(
        codeChallenge: String
    ): Uri =
      val params = Map(
        "client_id"             -> auth.clientId,
        "response_type"         -> "code",
        "code_challenge"        -> codeChallenge,
        "code_challenge_method" -> "plain",
        "redirect_uri"          -> auth.redirectUri.toString
      )
      uri"https://myanimelist.net/v1/oauth2/authorize?${params}"

    override val generateCodeChallenge: F[String] =
      (1 to 50).toList
        .traverse(_ => random.nextAlphaNumeric)
        .map(_.mkString)

  def makeUrl(mangaId: ExternalMangaId): String =
    s"https://myanimelist.net/manga/${mangaId}"
