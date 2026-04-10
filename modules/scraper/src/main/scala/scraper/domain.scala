package scraper
package domain

import java.net.URI
import java.time.LocalDate

import cats.data.NonEmptySet
import neotype.interop.cats.given
import cats.kernel.Order

type EntryNo = EntryNo.Type
object EntryNo extends neotype.Subtype[String]:
  val empty: EntryNo = EntryNo("")

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

object EntryFound:
  given Order[EntryFound] = Order.by[EntryFound, EntryTitle](_.title)

type JobLabel = JobLabel.Type
object JobLabel extends neotype.Subtype[Long]

type AuthorScrapingUri = AuthorScrapingUri.Type
object AuthorScrapingUri extends neotype.Subtype[URI]

type AssetTitle = AssetTitle.Type
object AssetTitle extends neotype.Subtype[String]

type Author = Author.Type
object Author extends neotype.Subtype[String]

case class AssetFound(
    assetTitle: AssetTitle,
    entries: NonEmptySet[EntryFound],
    authors: Set[Author]
)

object AssetFound:
  def ofSingleEntry(entry: EntryFound, authors: Set[Author]): AssetFound =
    AssetFound(
      AssetTitle.unsafeMake(entry.title),
      NonEmptySet.of(entry),
      authors
    )

enum ScrapeError:
  case NoEntriesFound
  case InvalidResource(message: String)
  case Other(message: String)

trait SiteScraper[F[_]]:
  def findEntries(uri: URI): F[Either[ScrapeError, List[EntryFound]]]

trait SiteScraperOfAuthor[F[_]]:
  def scrapeForAssets(
      uri: AuthorScrapingUri
  ): F[Either[ScrapeError, List[AssetFound]]]

type SuccessfulJob[A] = (JobLabel, List[A])

case class ScrapeResults(
    successfulAssetJobs: List[SuccessfulJob[EntryFound]],
    successfulAuthorJobs: List[SuccessfulJob[AssetFound]],
    failures: List[(JobLabel, ScrapeError)]
)

enum Instruction[F[_]]:
  // `uri` should change from `URI` to dedicated `AssetScrapingUri` type
  case ScrapeAsset[F[_]](label: JobLabel, uri: URI, scraper: SiteScraper[F])
      extends Instruction[F]
  case ScrapeAuthor[F[_]](
      label: JobLabel,
      uri: AuthorScrapingUri,
      scraper: SiteScraperOfAuthor[F]
  ) extends Instruction[F]
