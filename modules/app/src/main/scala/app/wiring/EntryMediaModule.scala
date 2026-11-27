package app.wiring

import cats.effect.IO
import doobie.Transactor
import entryMedia.*
import entryMedia.domain.Config
import http.View.NavBarItem

case class EntryMediaModule(controller: EntryMediaController)

object EntryMediaModule:
  def make(
      config: Config,
      navBarItems: List[NavBarItem],
      xa: Transactor[IO]
  ): EntryMediaModule =
    val repository = EntryMediaRepository.make
    val scanner    = EntryMediaScanner.make(repository, config, xa)
    val view       = EntryMediaView(navBarItems)
    EntryMediaModule(
      EntryMediaController(repository, scanner, view, config, xa)
    )
