package snapshot
package git

import scala.sys.process.*

import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{ Files, Path, Paths }
import java.time.{ LocalDate, ZoneId }

import cats.effect.kernel.{ Clock, Sync }
import cats.syntax.all.*
import core.Newtype
import java.io.File
import cats.data.EitherT

trait Snapshot[F[_]]:
  def save(): F[Either[Throwable, Unit]]

type PathToDb = PathToDb.Type
object PathToDb extends Newtype[String]

type PathToRepository = PathToRepository.Type
object PathToRepository extends Newtype[String]

type RecencyThreshold = RecencyThreshold.Type
object RecencyThreshold extends Newtype[Long]

extension [F[_]: Sync](path: Path)
  def toFileSafe = Sync[F].delay(path.toFile).attempt

class GitSnapshot[F[_]: Sync](
    pathToDb: PathToDb,
    pathToRepository: PathToRepository,
    recencyThreshold: RecencyThreshold
)(using clock: Clock[F]) extends Snapshot[F]:
  private val dbPath   = Paths.get(pathToDb)
  private val repoPath = Paths.get(pathToRepository)
  private val repoDir  = EitherT(repoPath.toFileSafe)

  def save(): F[Either[Throwable, Unit]] =
    (copyDbToRepo *> addFile *> commit *> publish).value

  def wasSavedRecently =
    for
      now <- clock.realTimeInstant
      ls  <- lastSaved
      daysDifference = ls.map: time =>
        val lastSavedDay =
          LocalDate.ofInstant(time.toInstant, ZoneId.systemDefault).toEpochDay
        val currentDay =
          LocalDate.ofInstant(now, ZoneId.systemDefault).toEpochDay
        currentDay - lastSavedDay
    yield daysDifference.map(_ < recencyThreshold).getOrElse(false)

  private def lastSaved =
    Sync[F]
      .delay(Files.getLastModifiedTime(dbPath))
      .attempt
      .map(_.toOption)

  private def copyDbToRepo =
    EitherT:
      Sync[F]
        .delay:
          // TODO: Add filename to repoPath when copying
          Files.copy(dbPath, repoPath, REPLACE_EXISTING)
        .attempt

  private def addFile =
    repoDir.flatMap(runCommand("git" :: "add" :: "???" :: Nil, _))
  private def commit = repoDir.flatMap(runCommand(
    "git" :: "commit" :: "-m" :: "'snapshot toshokan'" :: Nil,
    _
  ))
  private def publish = repoDir.flatMap(runCommand("git" :: "push" :: Nil, _))

  private def runCommand(
      command: List[String],
      directory: File
  ): EitherT[F, Throwable, Unit] =
    EitherT:
      Sync[F].delay:
        val result = Process(command, directory).!
        Either.cond(
          result == 0,
          (),
          new RuntimeException(
            s"Error: Command '$command' failed with exit code $result"
          )
        )
