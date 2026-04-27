package app.wiring

import cats.effect.IO
import doobie.Transactor
import myAnimeList.{MyAnimeListController, MyAnimeListService}

case class MyAnimeListModule(
    service: MyAnimeListService,
    controller: MyAnimeListController
)

object MyAnimeListModule:
  def make(
      externals: ExternalServices,
      xa: Transactor[IO]
  ): IO[MyAnimeListModule] =
    for service <- MyAnimeListService.make(xa, externals.malClient)
    yield MyAnimeListModule(
      service = service,
      controller = MyAnimeListController(service)
    )
