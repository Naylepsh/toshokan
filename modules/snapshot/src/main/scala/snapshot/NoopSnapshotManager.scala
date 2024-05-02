package snapshot

import cats.Applicative
import cats.syntax.all.*

class NoopSnapshotManager[F[_]](using F: Applicative[F])
    extends SnapshotManager[F]:
  override def save(): F[Either[Throwable, Unit]] = ().asRight.pure

  override def wasSavedRecently(): F[Boolean] = true.pure

  override def saveIfDue(): F[Unit] = ().pure
