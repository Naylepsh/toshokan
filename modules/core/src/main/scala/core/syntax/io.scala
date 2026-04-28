package core.syntax

import cats.effect.IO
import cats.mtl.Raise

extension [A](fa: IO[Option[A]])
  def someOrRaise[E](err: => E)(using Raise[IO, E]): IO[A] =
    fa.flatMap(_.orRaise(err))
