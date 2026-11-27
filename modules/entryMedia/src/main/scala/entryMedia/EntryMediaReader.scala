package entryMedia

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.IO
import entryMedia.domain.EntryMediaKind

case class Pages(count: Int, firstExt: String, firstBytes: IO[Array[Byte]])

object EntryMediaReader:
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

  /** Image file names in reading order */
  def pageNames(path: Path, kind: EntryMediaKind): IO[List[String]] =
    IO.blocking:
      kind match
        case EntryMediaKind.Folder  => folderPageNames(path)
        case EntryMediaKind.Archive => archivePageNames(path)

  def readPage(
      path: Path,
      kind: EntryMediaKind,
      name: String
  ): IO[Array[Byte]] =
    IO.blocking:
      kind match
        case EntryMediaKind.Folder => Files.readAllBytes(path.resolve(name))
        case EntryMediaKind.Archive =>
          Using.resource(ZipFile(path.toFile)): zip =>
            Using.resource(zip.getInputStream(zip.getEntry(name)))(
              _.readAllBytes()
            )

  /** None when the path holds no images */
  def inspect(path: Path, kind: EntryMediaKind): IO[Option[Pages]] =
    pageNames(path, kind).map: names =>
      names.headOption.map: first =>
        Pages(names.size, extensionOf(first), readPage(path, kind, first))

  private def folderPageNames(dir: Path): List[String] =
    Using
      .resource(Files.list(dir)): stream =>
        stream
          .iterator()
          .asScala
          .filter(Files.isRegularFile(_))
          .map(_.getFileName.toString)
          .filter(isImage)
          .toList
      .sorted(using naturalOrder)

  // shortcut: only top-level zip entries count as pages, upgrade if archives wrap images in a folder
  private def archivePageNames(file: Path): List[String] =
    Using
      .resource(ZipFile(file.toFile)): zip =>
        zip
          .entries()
          .asScala
          .filter(e => !e.isDirectory && !e.getName.contains('/'))
          .map(_.getName)
          .filter(isImage)
          .toList
      .sorted(using naturalOrder)
