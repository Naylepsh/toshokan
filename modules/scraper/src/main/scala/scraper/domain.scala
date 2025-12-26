package scraper
package domain

import java.net.URI
import java.time.LocalDate

type EntryNo = EntryNo.Type
object EntryNo extends neotype.Subtype[String]

type EntryTitle = EntryTitle.Type
object EntryTitle extends neotype.Subtype[String]

type EntryUri = EntryUri.Type
object EntryUri extends neotype.Subtype[URI]:
  def apply(value: String): Either[String, EntryUri] =
    core.Uri.fromStringSafe(value).map(EntryUri(_))

type DateUploaded = DateUploaded.Type
object DateUploaded extends neotype.Subtype[LocalDate]

case class EntryFound(
    title: EntryTitle,
    no: EntryNo,
    uri: EntryUri,
    dateUploaded: DateUploaded
)

type JobLabel = JobLabel.Type
object JobLabel extends neotype.Subtype[Long]

enum ScrapeError:
  case NoEntriesFound
  case InvalidResource(message: String)
  case Other(message: String)

trait SiteScraper[F[_]]:
  def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]]
