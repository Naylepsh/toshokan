package app
package logging

import cats.effect.IO

def init: IO[Unit] =
  IO.delay:
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(minimumLevel = Some(scribe.Level.Debug))
      .replace()
  .void
