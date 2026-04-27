package snapshot

import cats.effect.IO

trait SnapshotManager:
  def save(): IO[Unit]
  def wasSavedRecently(): IO[Boolean]
  def saveIfDue(): IO[Unit]
