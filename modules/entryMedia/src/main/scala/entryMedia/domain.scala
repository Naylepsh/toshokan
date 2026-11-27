package entryMedia
package domain

import java.nio.file.Path

type EntryMediaId = EntryMediaId.Type
object EntryMediaId extends neotype.Newtype[Long]

type EntryMediaName = EntryMediaName.Type
object EntryMediaName extends neotype.Subtype[String]

enum EntryMediaKind:
  case Folder, Archive

case class EntryMedia(
    id: EntryMediaId,
    name: EntryMediaName,
    kind: EntryMediaKind,
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

case class ScanResult(seen: Int, duplicates: List[EntryMediaName])
