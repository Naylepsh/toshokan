package snapshot
package git

import java.io.File
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Path, Paths}
import java.time.{LocalDate, ZoneId}

import scala.sys.process.*

import cats.effect.kernel.{Clock, Sync}
import cats.syntax.all.*
import core.Newtype

type PathToSnapshot = PathToSnapshot.Type
object PathToSnapshot extends Newtype[String]

type RecencyThreshold = RecencyThreshold.Type
object RecencyThreshold extends Newtype[Long]

extension [F[_]: Sync](path: Path)
  def toFileSafe: F[File] = Sync[F].delay(path.toFile)

case class Config(
    pathToDb: db.Path,
    pathToSnapshot: PathToSnapshot,
    recencyThreshold: RecencyThreshold
)

class GitSnapshotManager[F[_]: Sync](config: Config)(using clock: Clock[F])
    extends SnapshotManager[F]:
  private val dbPath       = Paths.get(config.pathToDb.withoutProtocol)
  private val snapshotPath = Paths.get(config.pathToSnapshot)
  private val snapshotDir =
    snapshotPath.toFileSafe.map(_.getParentFile)

  def saveIfDue(): F[Unit] =
    wasSavedRecently().flatMap:
      case true =>
        scribe.cats[F].debug("Snapshot taken recently. Not taking another one")
      case false =>
        scribe
          .cats[F]
          .info("Taking a snapshot")
          .flatMap: _ =>
            save().attempt.flatMap:
              case Left(error) =>
                scribe.cats[F].error(error.toString)
              case Right(_) =>
                scribe.cats[F].info("Saved the snapshot")

  def save(): F[Unit] =
    copyDbToRepo *> addFile *> commit *> publish

  def wasSavedRecently(): F[Boolean] =
    for
      now <- clock.realTimeInstant
      ls  <- lastSaved
      daysDifference = ls.map: time =>
        val lastSavedDay =
          LocalDate
            .ofInstant(time.toInstant, ZoneId.systemDefault)
            .toEpochDay
        val currentDay =
          LocalDate
            .ofInstant(now, ZoneId.systemDefault)
            .toEpochDay
        currentDay - lastSavedDay
    yield daysDifference.map(_ < config.recencyThreshold).getOrElse(false)

  private def lastSaved =
    Sync[F]
      .delay(Files.getLastModifiedTime(snapshotPath))
      .attempt
      .map(_.toOption)

  private def copyDbToRepo =
    Sync[F].delay(Files.copy(dbPath, snapshotPath, REPLACE_EXISTING))

  private def addFile = snapshotDir.flatMap: repo =>
    runCommand(List("git", "add", snapshotPath.getFileName.toString), repo)

  private def commit = snapshotDir.flatMap: repo =>
    runCommand(List("git", "commit", "-m", "'snapshot toshokan'"), repo)

  private def publish = snapshotDir.flatMap: repo =>
    runCommand(List("git", "push"), repo)

  private def runCommand(
      command: List[String],
      directory: File
  ): F[Unit] =
    Sync[F].defer:
      val result = Process(command, directory).!
      Either
        .cond(
          result == 0,
          (),
          new RuntimeException(
            s"Error: Command '$command' failed with exit code $result"
          )
        )
        .liftTo[F]
