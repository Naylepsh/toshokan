package assetScraping.scrapes.domain

import java.time.LocalDate

import core.{Newtype, given}
import library.domain.AssetId
import org.typelevel.cats.time.*

case class ScrapingSummary(
    newEntriesCount: Int,
    configsCount: Int,
    errorsCount: Int,
    scrapingTimeSeconds: Long,
    savingTimeSeconds: Long
)

type PastScrapeCreatedAt = PastScrapeCreatedAt.Type
object PastScrapeCreatedAt extends Newtype[LocalDate]

case class PastScrape(assetId: AssetId, createdAt: PastScrapeCreatedAt)
