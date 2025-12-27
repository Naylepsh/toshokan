package app.wiring

import cats.effect.IO
import doobie.Transactor
import myAnimeList.{MyAnimeListController, MyAnimeListService}

case class MyAnimeListModule[F[_]](
    service: MyAnimeListService[F],
    controller: MyAnimeListController[F]
)

object MyAnimeListModule:
  def make(
      externals: ExternalServices[IO],
      xa: Transactor[IO]
  ): IO[MyAnimeListModule[IO]] =
    for
      service <- MyAnimeListService.make[IO](xa, externals.malClient)
      controller = MyAnimeListController(service)
    yield MyAnimeListModule(
      service = service,
      controller = controller
    )
