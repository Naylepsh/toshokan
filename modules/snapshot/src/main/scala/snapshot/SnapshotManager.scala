package snapshot

trait SnapshotManager[F[_]]:
  def save(): F[Either[Throwable, Unit]]
  def wasSavedRecently(): F[Boolean]
  def saveIfDue(): F[Unit]
