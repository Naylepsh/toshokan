package myAnimeList

import java.net.URI

import cats.effect.IO
import cats.effect.std.Random
import cats.syntax.all.*
import sttp.capabilities.WebSockets
import sttp.client3.circe.*
import sttp.client3.{SttpBackend, UriContext, basicRequest}
import sttp.model.{StatusCode, Uri}

import domain.{ExternalMangaId, LatestChapter, Term}
import schemas.*

case class MalAuth(clientId: String, clientSecret: String, redirectUri: URI)

trait MyAnimeListClient:
  def searchManga(token: AuthToken, term: Term.Name): IO[GetMangaListSuccess]
  def find(token: AuthToken, mangaId: ExternalMangaId): IO[Option[Manga]]
  def updateStatus(token: AuthToken, mangaId: ExternalMangaId, latestChapter: LatestChapter): IO[Unit]
  def refreshAuthToken(token: RefreshToken): IO[AuthToken]
  def acquireToken(code: String, codeChallenge: String): IO[AuthToken]
  def generateCodeChallenge: IO[String]
  def createAuthorizationLink(codeChallenge: String): Uri

object MyAnimeListClient:
  def make(
      backend: SttpBackend[IO, WebSockets],
      auth: MalAuth,
      random: Random[IO]
  ): MyAnimeListClient = new:
    override def searchManga(token: AuthToken, term: Term.Name): IO[GetMangaListSuccess] =
      val url = uri"https://api.myanimelist.net/v2/manga?q=$term"
      basicRequest.get(url).auth.bearer(token.accessToken)
        .response(asJson[GetMangaListSuccess])
        .send(backend).map(_.body).rethrow

    override def find(token: AuthToken, mangaId: ExternalMangaId): IO[Option[Manga]] =
      val url = uri"https://api.myanimelist.net/v2/manga/$mangaId"
      basicRequest.get(url).auth.bearer(token.accessToken)
        .response(asJson[Manga])
        .send(backend)
        .flatMap: response =>
          response.code match
            case StatusCode.NotFound => IO.pure(None)
            case _ => response.body.leftMap(new RuntimeException(_)).liftTo[IO].map(_.some)

    override def updateStatus(
        token: AuthToken,
        mangaId: ExternalMangaId,
        latestChapter: LatestChapter
    ): IO[Unit] =
      val url = uri"https://api.myanimelist.net/v2/manga/$mangaId/my_list_status"
      basicRequest
        .patch(url)
        .body("status" -> MangaStatus.Reading.urlEncoded, "num_chapters_read" -> latestChapter.toString)
        .auth.bearer(token.accessToken)
        .send(backend)
        .flatMap: response =>
          response.body.void.leftMap(new RuntimeException(_)).liftTo[IO]

    override def refreshAuthToken(token: RefreshToken): IO[AuthToken] =
      val url = uri"https://myanimelist.net/v1/oauth2/token"
      basicRequest.post(url)
        .body(Map(
          "client_id" -> auth.clientId, "client_secret" -> auth.clientSecret,
          "grant_type" -> "refresh_token", "refresh_token" -> token
        ))
        .response(asJson[AuthToken]).send(backend)
        .flatMap(_.body.leftMap(new RuntimeException(_)).liftTo[IO])

    override def acquireToken(code: String, codeChallenge: String): IO[AuthToken] =
      val url = uri"https://myanimelist.net/v1/oauth2/token"
      basicRequest.post(url)
        .body(Map(
          "client_id" -> auth.clientId, "client_secret" -> auth.clientSecret,
          "grant_type" -> "authorization_code", "code" -> code,
          "code_verifier" -> codeChallenge, "redirect_uri" -> auth.redirectUri.toString
        ))
        .response(asJson[AuthToken]).send(backend)
        .flatMap(_.body.leftMap(new RuntimeException(_)).liftTo[IO])

    override def createAuthorizationLink(codeChallenge: String): Uri =
      val params = Map(
        "client_id" -> auth.clientId, "response_type" -> "code",
        "code_challenge" -> codeChallenge, "code_challenge_method" -> "plain",
        "redirect_uri" -> auth.redirectUri.toString
      )
      uri"https://myanimelist.net/v1/oauth2/authorize?${params}"

    override val generateCodeChallenge: IO[String] =
      (1 to 50).toList.traverse(_ => random.nextAlphaNumeric).map(_.mkString)

  def makeUrl(mangaId: ExternalMangaId): String =
    s"https://myanimelist.net/manga/${mangaId}"
