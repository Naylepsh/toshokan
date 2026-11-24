package app.wiring

import cats.effect.IO
import doobie.Transactor
import externalChapters.*
import externalChapters.domain.Config
import http.View.NavBarItem

case class ExternalChaptersModule(controller: ExternalChapterController)

object ExternalChaptersModule:
  def make(
      config: Config,
      navBarItems: List[NavBarItem],
      xa: Transactor[IO]
  ): ExternalChaptersModule =
    val repository = ExternalChapterRepository.make
    val scanner    = ExternalChapterScanner.make(repository, config, xa)
    val view       = ExternalChapterView(navBarItems)
    ExternalChaptersModule(
      ExternalChapterController(repository, scanner, view, config, xa)
    )
