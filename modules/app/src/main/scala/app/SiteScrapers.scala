package app

import assetScraping.domain.Site
import cats.effect.kernel.Sync
import com.microsoft.playwright.Browser
import scraper.domain.SiteScraper
import scraper.sites.hitomi.HitomiScraper
import scraper.sites.mangadex.{MangadexApi, MangadexScraper}
import scraper.sites.mangakakalot.MangakakalotScraper
import scraper.sites.yatta.YattaScraper
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend

object SiteScrapers:
  def makeScraperPicker[F[_]: Sync](
      backend: SttpBackend[F, WebSockets],
      browser: Browser
  ) =
    val mangadexApi         = MangadexApi.make(backend)
    val mangadexScraper     = MangadexScraper[F](mangadexApi)
    val mangakakalotScraper = MangakakalotScraper[F]()
    val yattaScraper        = YattaScraper[F]()
    val hitomiScraper       = HitomiScraper[F](browser, timeout = 10_000)

    val pickSiteScraper: Site => SiteScraper[F] =
      case Site.Mangadex     => mangadexScraper
      case Site.Mangakakalot => mangakakalotScraper
      case Site.Yatta        => yattaScraper
      case Site.Hitomi       => hitomiScraper

    pickSiteScraper
