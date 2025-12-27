package app.wiring

import cats.effect.IO
import cats.effect.std.Random
import mangadex.MangadexApi
import myAnimeList.{MalAuth, MyAnimeListClient}
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend

case class ExternalServices[F[_]](
    mangadexApi: MangadexApi[F],
    malClient: Option[MyAnimeListClient[F]]
)

object ExternalServices:
  def make(
      httpBackend: SttpBackend[IO, WebSockets],
      malAuth: Option[MalAuth],
      random: Random[IO]
  ): ExternalServices[IO] =
    val mangadexApi = MangadexApi.make[IO](httpBackend)
    val malClient =
      malAuth.map(MyAnimeListClient.make[IO](httpBackend, _, random))

    ExternalServices(
      mangadexApi = mangadexApi,
      malClient = malClient
    )
