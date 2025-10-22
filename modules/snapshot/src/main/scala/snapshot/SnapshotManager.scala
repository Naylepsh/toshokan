package snapshot

trait SnapshotManager[F[_]]:
  def save(): F[Unit]
  def wasSavedRecently(): F[Boolean]
  def saveIfDue(): F[Unit]
