package core

import scala.concurrent.duration.FiniteDuration

import cats.effect.{ Clock, Sync }
import cats.syntax.all.*

object Measure:
  def of[F[_]: Sync, A](fa: F[A])(using
  clock: Clock[F]): F[(A, FiniteDuration)] =
    for
      start  <- clock.monotonic
      result <- fa
      finish <- clock.monotonic
    yield (result, finish - start)

  extension [F[_]: Sync: Clock, A](fa: F[A])
    def measure: F[(A, FiniteDuration)] = Measure.of(fa)
