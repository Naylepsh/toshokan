package app.wiring

import cats.effect.IO
import cats.effect.std.Random
import mangadex.MangadexApi
import myAnimeList.{MalAuth, MyAnimeListClient}
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend

case class ExternalServices(
    mangadexApi: MangadexApi,
    malClient: Option[MyAnimeListClient]
)

object ExternalServices:
  def make(
      httpBackend: SttpBackend[IO, WebSockets],
      malAuth: Option[MalAuth],
      random: Random[IO]
  ): ExternalServices =
    val mangadexApi = MangadexApi.make(httpBackend)
    val malClient = malAuth.map(MyAnimeListClient.make(httpBackend, _, random))

    ExternalServices(
      mangadexApi = mangadexApi,
      malClient = malClient
    )
