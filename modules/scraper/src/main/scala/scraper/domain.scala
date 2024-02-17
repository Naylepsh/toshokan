package scraper

import java.net.URI
import java.time.LocalDate

import core.{ Newtype, given }
import doobie.implicits.legacy.localdate.*
import org.typelevel.cats.time.*

object domain:
  type EntryNo = EntryNo.Type
  object EntryNo extends Newtype[String]

  type EntryUri = EntryUri.Type
  object EntryUri extends Newtype[URI]

  type DateUploaded = DateUploaded.Type
  object DateUploaded extends Newtype[LocalDate]

  case class EntryFound(no: EntryNo, uri: EntryUri, dateUploaded: DateUploaded)

  type JobLabel = JobLabel.Type
  object JobLabel extends Newtype[Long]

  enum ScrapeError:
    case NoEntriesFound
