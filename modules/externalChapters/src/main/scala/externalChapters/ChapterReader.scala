package externalChapters

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.IO
import externalChapters.domain.ChapterKind

case class Pages(count: Int, firstExt: String, firstBytes: IO[Array[Byte]])

object ChapterReader:
  private val imageExts = Set("jpg", "jpeg", "png", "webp", "gif")

  def extensionOf(fileName: String): String =
    fileName.lastIndexOf('.') match
      case -1 => ""
      case i  => fileName.substring(i + 1).toLowerCase

  private def isImage(fileName: String) =
    imageExts.contains(extensionOf(fileName))

  /** Compares digit runs numerically so that `2.jpg` comes before `10.jpg` */
  val naturalOrder: Ordering[String] = (a, b) =>
    val chunks = "\\d+|\\D+".r
    chunks
      .findAllIn(a.toLowerCase)
      .toList
      .zipAll(chunks.findAllIn(b.toLowerCase).toList, "", "")
      .iterator
      .map: (x, y) =>
        if x.headOption.exists(_.isDigit) && y.headOption.exists(_.isDigit)
        then BigInt(x).compare(BigInt(y))
        else x.compareTo(y)
      .find(_ != 0)
      .getOrElse(0)

  /** None when the path holds no images */
  def inspect(path: Path, kind: ChapterKind): IO[Option[Pages]] =
    IO.blocking:
      kind match
        case ChapterKind.Folder  => inspectFolder(path)
        case ChapterKind.Archive => inspectArchive(path)

  private def inspectFolder(dir: Path): Option[Pages] =
    val images = Using.resource(Files.list(dir)): stream =>
      stream
        .iterator()
        .asScala
        .filter(p => Files.isRegularFile(p) && isImage(p.getFileName.toString))
        .map(_.getFileName.toString)
        .toList
        .sorted(using naturalOrder)
    images.headOption.map: first =>
      Pages(
        images.size,
        extensionOf(first),
        IO.blocking(Files.readAllBytes(dir.resolve(first)))
      )

  // shortcut: only top-level zip entries count as pages, upgrade if archives wrap images in a folder
  private def inspectArchive(file: Path): Option[Pages] =
    val images = Using.resource(ZipFile(file.toFile)): zip =>
      zip
        .entries()
        .asScala
        .filter(e => !e.isDirectory && !e.getName.contains('/'))
        .map(_.getName)
        .filter(isImage)
        .toList
        .sorted(using naturalOrder)
    images.headOption.map: first =>
      Pages(
        images.size,
        extensionOf(first),
        IO.blocking:
          Using.resource(ZipFile(file.toFile)): zip =>
            Using.resource(zip.getInputStream(zip.getEntry(first)))(
              _.readAllBytes()
            )
      )
