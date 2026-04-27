package app.wiring

import assetScraping.configs.domain.{AuthorSite, Site}
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
    forAsset: Site => SiteScraper,
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

    val pickAssetScraper: Site => SiteScraper =
      case Site.Mangadex     => mangadexScraper
      case Site.Mangakakalot => mangakakalotScraper
      case Site.Yatta        => yattaScraper
      case Site.Empik        => empikScraper
      case Site.DynastyScans => dynastyScansScraper
      case Site.Batoto       => batotoScraper
      case _                 => ???

    val pickAuthorScraper: AuthorSite => SiteScraperOfAuthor =
      case AuthorSite.Hitomi => hitomiScraper

    SiteScrapers(pickAssetScraper, pickAuthorScraper)
