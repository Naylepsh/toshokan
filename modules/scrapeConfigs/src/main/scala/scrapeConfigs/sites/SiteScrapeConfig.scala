package scrapeConfigs.sites

import scrapeConfigs.domain.ScrapeAssetConfigUri
import java.util.Date

case class FindEntryResult(no: String, dateUploaded: Date)

trait SiteScrapeConfig[F[_]]:
  def findEntries(uri: ScrapeAssetConfigUri): F[Either[String, List[FindEntryResult]]]
