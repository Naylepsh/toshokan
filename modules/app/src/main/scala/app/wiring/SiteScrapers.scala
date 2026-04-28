package app.wiring

import assetScraping.configs.domain.{AssetSite, AuthorSite}
import cats.effect.IO
import com.microsoft.playwright.Browser
import mangadex.MangadexApi
import scraper.domain.{SiteScraper, SiteScraperOfAuthor}
import scraper.sites.batoto.BatotoScraper
import scraper.sites.dynastyScans.DynastyScansScraper
import scraper.sites.empik.EmpikScraper
import scraper.sites.hitomi.HitomiScraper
import scraper.sites.mangadex.MangadexScraper
import scraper.sites.mangakakalot.MangakakalotScraper
import scraper.sites.yatta.YattaScraper
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend

case class SiteScrapers(
    forAsset: AssetSite => SiteScraper,
    forAuthor: AuthorSite => SiteScraperOfAuthor
)

object SiteScrapers:
  def make(
      backend: SttpBackend[IO, WebSockets],
      browser: Browser
  ): SiteScrapers =
    val mangadexApi         = MangadexApi.make(backend)
    val mangadexScraper     = MangadexScraper(mangadexApi)
    val mangakakalotScraper = MangakakalotScraper()
    val yattaScraper        = YattaScraper()
    val hitomiScraper       = HitomiScraper(browser, timeout = 10_000)
    val empikScraper        = EmpikScraper()
    val dynastyScansScraper = DynastyScansScraper()
    val batotoScraper       = BatotoScraper()

    val pickAssetScraper: AssetSite => SiteScraper =
      case AssetSite.Mangadex     => mangadexScraper
      case AssetSite.Mangakakalot => mangakakalotScraper
      case AssetSite.Yatta        => yattaScraper
      case AssetSite.Empik        => empikScraper
      case AssetSite.DynastyScans => dynastyScansScraper
      case AssetSite.Batoto       => batotoScraper

    val pickAuthorScraper: AuthorSite => SiteScraperOfAuthor =
      case AuthorSite.Hitomi => hitomiScraper

    SiteScrapers(pickAssetScraper, pickAuthorScraper)
