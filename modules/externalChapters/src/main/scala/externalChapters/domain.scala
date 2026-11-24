package externalChapters
package domain

import java.nio.file.Path

type ChapterId = ChapterId.Type
object ChapterId extends neotype.Newtype[Long]

type ChapterName = ChapterName.Type
object ChapterName extends neotype.Subtype[String]

enum ChapterKind:
  case Folder, Archive

case class ExternalChapter(
    id: ChapterId,
    name: ChapterName,
    kind: ChapterKind,
    path: String,
    pageCount: Int,
    modifiedAt: Long,
    size: Long,
    thumbnailExt: Option[String],
    missingSince: Option[Long]
)

case class Config(roots: List[Path], thumbnailDir: Path)

enum ScanError:
  case RootUnreadable(root: Path)

case class ScanResult(seen: Int, duplicates: List[ChapterName])
