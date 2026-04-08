package app.wiring

import assetScraping.configs.domain.{AuthorSite, Site}
import cats.effect.Async
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

case class SiteScrapers[F[_]](
    forAsset: Site => SiteScraper[F],
    forAuthor: AuthorSite => SiteScraperOfAuthor[F]
)

object SiteScrapers:
  def make[F[_]: Async](
      backend: SttpBackend[F, WebSockets],
      browser: Browser
  ): SiteScrapers[F] =
    val mangadexApi         = MangadexApi.make[F](backend)
    val mangadexScraper     = MangadexScraper[F](mangadexApi)
    val mangakakalotScraper = MangakakalotScraper[F]()
    val yattaScraper        = YattaScraper[F]()
    val hitomiScraper       = HitomiScraper[F](browser, timeout = 10_000)
    val empikScraper        = EmpikScraper[F]()
    val dynastyScansScraper = DynastyScansScraper[F]()
    val batotoScraper       = BatotoScraper[F]()

    val pickAssetScraper: Site => SiteScraper[F] =
      case Site.Mangadex     => mangadexScraper
      case Site.Mangakakalot => mangakakalotScraper
      case Site.Yatta        => yattaScraper
      case Site.Empik        => empikScraper
      case Site.DynastyScans => dynastyScansScraper
      case Site.Batoto       => batotoScraper
      // TODO: Remove this `???` once hitomi is removed from the AssetSites
      case _ => ???

    val pickAuthorScraper: AuthorSite => SiteScraperOfAuthor[F] =
      case AuthorSite.Hitomi => hitomiScraper

    SiteScrapers(pickAssetScraper, pickAuthorScraper)
