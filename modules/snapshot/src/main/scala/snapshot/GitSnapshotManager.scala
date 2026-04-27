package snapshot
package git

import java.io.File
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Paths}
import java.time.{LocalDate, ZoneId}

import scala.sys.process.*

import cats.effect.IO
import cats.syntax.all.*

type PathToSnapshot = PathToSnapshot.Type
object PathToSnapshot extends neotype.Subtype[String]

type RecencyThreshold = RecencyThreshold.Type
object RecencyThreshold extends neotype.Subtype[Long]

case class Config(
    pathToDb: db.Path,
    pathToSnapshot: PathToSnapshot,
    recencyThreshold: RecencyThreshold
)

class GitSnapshotManager(config: Config) extends SnapshotManager:
  private val dbPath       = Paths.get(config.pathToDb.withoutProtocol)
  private val snapshotPath = Paths.get(config.pathToSnapshot)
  private val snapshotDir  = IO.delay(snapshotPath.toFile.getParentFile)

  def saveIfDue(): IO[Unit] =
    wasSavedRecently().flatMap:
      case true =>
        scribe.cats[IO].debug("Snapshot taken recently. Not taking another one")
      case false =>
        scribe.cats[IO].info("Taking a snapshot")
          *> save().attempt.flatMap:
            case Left(error) => scribe.cats[IO].error(error.toString)
            case Right(_)    => scribe.cats[IO].info("Saved the snapshot")

  def save(): IO[Unit] =
    copyDbToRepo *> addFile *> commit *> publish

  def wasSavedRecently(): IO[Boolean] =
    for
      now <- IO.realTimeInstant
      ls  <- lastSaved
      daysDifference = ls.map: time =>
        val lastSavedDay =
          LocalDate.ofInstant(time.toInstant, ZoneId.systemDefault).toEpochDay
        val currentDay =
          LocalDate.ofInstant(now, ZoneId.systemDefault).toEpochDay
        currentDay - lastSavedDay
    yield daysDifference.map(_ < config.recencyThreshold).getOrElse(false)

  private def lastSaved =
    IO.delay(Files.getLastModifiedTime(snapshotPath)).attempt.map(_.toOption)

  private def copyDbToRepo =
    IO.delay(Files.copy(dbPath, snapshotPath, REPLACE_EXISTING))

  private def addFile = snapshotDir.flatMap: repo =>
    runCommand(List("git", "add", snapshotPath.getFileName.toString), repo)

  private def commit = snapshotDir.flatMap: repo =>
    runCommand(List("git", "commit", "-m", "'snapshot toshokan'"), repo)

  private def publish = snapshotDir.flatMap: repo =>
    runCommand(List("git", "push"), repo)

  private def runCommand(command: List[String], directory: File): IO[Unit] =
    IO.defer:
      val result = Process(command, directory).!
      Either
        .cond(
          result == 0,
          (),
          new RuntimeException(
            s"Error: Command '$command' failed with exit code $result"
          )
        )
        .liftTo[IO]
