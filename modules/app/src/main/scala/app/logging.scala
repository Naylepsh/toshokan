package app
package logging

import cats.effect.kernel.Sync
import cats.syntax.all.*

def init[F[_]: Sync]: F[Unit] =
  Sync[F]
    .delay(
      scribe.Logger.root
        .clearHandlers()
        .clearModifiers()
        .withHandler(minimumLevel = Some(scribe.Level.Debug))
        .replace()
    )
    .void
