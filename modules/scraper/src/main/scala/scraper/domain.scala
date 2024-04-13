package scraper
package domain

import java.net.URI
import java.time.LocalDate

import core.{ Newtype, given }
import org.typelevel.cats.time.*

type EntryNo = EntryNo.Type
object EntryNo extends Newtype[String]

type EntryUri = EntryUri.Type
object EntryUri extends Newtype[URI]:
  def apply(value: String): Either[String, EntryUri] =
    core.Uri.fromStringSafe(value).map(EntryUri(_))

type DateUploaded = DateUploaded.Type
object DateUploaded extends Newtype[LocalDate]

case class EntryFound(no: EntryNo, uri: EntryUri, dateUploaded: DateUploaded)

type JobLabel = JobLabel.Type
object JobLabel extends Newtype[Long]

enum ScrapeError:
  case NoEntriesFound
  case InvalidResource(message: String)
  case Other(message: String)

trait SiteScraper[F[_]]:
  def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]]
