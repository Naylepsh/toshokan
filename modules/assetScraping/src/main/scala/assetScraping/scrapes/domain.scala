package assetScraping.scrapes.domain

case class ScrapingSummary(
    newEntriesCount: Int,
    configsCount: Int,
    errorsCount: Int,
    scrapingTimeSeconds: Long,
    savingTimeSeconds: Long
)
