package core.syntax

import cats.effect.IO
import cats.mtl.Raise
import cats.mtl.syntax.all.*

extension [A](opt: Option[A])
  def orRaise[E](err: => E)(using Raise[IO, E]): IO[A] =
    opt match
      case Some(a) => IO.pure(a)
      case None    => err.raise
