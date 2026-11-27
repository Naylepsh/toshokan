package externalChapters

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.IO
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import externalChapters.domain.*

trait ExternalChapterScanner:
  def scan: Raise[IO, ScanError] ?=> IO[ScanResult]

object ExternalChapterScanner:
  private case class Candidate(
      name: ChapterName,
      kind: ChapterKind,
      path: Path
  )

  def make(
      repository: ExternalChapterRepository,
      config: Config,
      xa: Transactor[IO]
  ): ExternalChapterScanner = new:
    override def scan: Raise[IO, ScanError] ?=> IO[ScanResult] =
      for
        _ <- config.roots.traverse_ : root =>
          IO.blocking(Files.isDirectory(root))
            .ifM(IO.unit, ScanError.RootUnreadable(root).raise)
        found <- config.roots.flatTraverse(candidatesIn)
        (candidates, duplicates) = dedupe(found)
        existing <- repository.findAll.transact(xa)
        existingByName = existing.map(c => c.name -> c).toMap
        seen <- candidates.flatTraverse(c => sync(c, existingByName.get))
        now  <- IO.realTime.map(_.toMillis)
        gone = existing.filter(c =>
          c.missingSince.isEmpty && !seen.contains(c.name)
        )
        _ <- repository.markMissing(gone.map(_.id), now).transact(xa)
        _ <- duplicates.traverse_(d =>
          scribe.cats[IO].warn(s"Duplicate external chapter ignored: $d")
        )
      yield ScanResult(seen.size, duplicates)

    private def candidatesIn(root: Path): IO[List[Candidate]] =
      IO.blocking:
        Using
          .resource(Files.list(root))(_.iterator().asScala.toList)
          .sortBy(_.getFileName.toString)
          .flatMap: path =>
            val fileName = path.getFileName.toString
            if Files.isDirectory(path) then
              List(Candidate(ChapterName(fileName), ChapterKind.Folder, path))
            else if Set("zip", "cbz").contains(
                ChapterReader.extensionOf(fileName)
              )
            then
              List(
                Candidate(
                  ChapterName(fileName.substring(0, fileName.lastIndexOf('.'))),
                  ChapterKind.Archive,
                  path
                )
              )
            else Nil

    /** First found wins */
    private def dedupe(
        found: List[Candidate]
    ): (List[Candidate], List[ChapterName]) =
      val (firsts, dupes) = found.foldLeft(
        (Map.empty[ChapterName, Candidate], List.empty[ChapterName])
      ):
        case ((acc, dupes), c) =>
          if acc.contains(c.name) then (acc, c.name :: dupes)
          else (acc + (c.name -> c), dupes)
      (found.filter(c => firsts.get(c.name).contains(c)), dupes.reverse)

    /** Returns the name when the candidate is a chapter */
    private def sync(
        candidate: Candidate,
        existing: ChapterName => Option[ExternalChapter]
    ): IO[List[ChapterName]] =
      for
        (modifiedAt, size) <- IO.blocking:
          (
            Files.getLastModifiedTime(candidate.path).toMillis,
            if candidate.kind == ChapterKind.Archive
            then Files.size(candidate.path)
            else 0L
          )
        known = existing(candidate.name)
        unchanged <- known.traverse: c =>
          thumbnailExists(c).map: hasThumbnail =>
            hasThumbnail && c.kind == candidate.kind &&
              c.modifiedAt == modifiedAt && c.size == size
        result <- (known, unchanged) match
          case (Some(c), Some(true)) =>
            upsert(candidate, c.pageCount, modifiedAt, size, c.thumbnailExt)
              .as(List(candidate.name))
          case _ =>
            ChapterReader
              .inspect(candidate.path, candidate.kind)
              .flatMap:
                case None => IO.pure(Nil)
                case Some(pages) =>
                  for
                    id <- upsert(
                      candidate,
                      pages.count,
                      modifiedAt,
                      size,
                      Some(pages.firstExt)
                    )
                    bytes <- pages.firstBytes
                    _ <- IO.blocking:
                      Files.createDirectories(config.thumbnailDir)
                      Files.write(thumbnailPath(id, pages.firstExt), bytes)
                  yield List(candidate.name)
      yield result

    private def upsert(
        candidate: Candidate,
        pageCount: Int,
        modifiedAt: Long,
        size: Long,
        thumbnailExt: Option[String]
    ) =
      repository
        .upsert(
          candidate.name,
          candidate.kind,
          candidate.path.toString,
          pageCount,
          modifiedAt,
          size,
          thumbnailExt
        )
        .transact(xa)

    private def thumbnailExists(chapter: ExternalChapter): IO[Boolean] =
      chapter.thumbnailExt
        .traverse(ext =>
          IO.blocking(Files.exists(thumbnailPath(chapter.id, ext)))
        )
        .map(_.getOrElse(false))

    private def thumbnailPath(id: ChapterId, ext: String) =
      ExternalChapterScanner.thumbnailPath(config.thumbnailDir, id, ext)

  def thumbnailPath(dir: Path, id: ChapterId, ext: String): Path =
    dir.resolve(s"$id.$ext")
