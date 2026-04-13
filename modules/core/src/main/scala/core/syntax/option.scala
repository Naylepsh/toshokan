package core.syntax

import cats.Applicative
import cats.mtl.Raise
import cats.syntax.applicative.*

def fromOption[F[_]: Applicative, E, A](
    opt: Option[A],
    err: E
)(using R: Raise[F, E]): F[A] =
  opt match
    case Some(a) => a.pure[F]
    case None    => R.raise(err)

extension [F[_]: Applicative, A](opt: Option[A])
  def orRaise[E](err: => E)(using Raise[F, E]): F[A] =
    fromOption(opt, err)
