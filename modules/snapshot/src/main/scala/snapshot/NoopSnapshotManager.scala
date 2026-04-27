package snapshot

import cats.effect.IO

class NoopSnapshotManager extends SnapshotManager:
  override def save(): IO[Unit]                = IO.unit
  override def wasSavedRecently(): IO[Boolean] = IO.pure(true)
  override def saveIfDue(): IO[Unit]           = IO.unit
