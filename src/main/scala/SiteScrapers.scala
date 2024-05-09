import assetScraping.domain.Site
import cats.effect.kernel.Sync
import scraper.domain.SiteScraper
import scraper.sites.mangadex.{ MangadexApi, MangadexScraper }
import scraper.sites.mangakakalot.MangakakalotScraper
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend

object SiteScrapers:
  def makeScraperPicker[F[_]: Sync](backend: SttpBackend[F, WebSockets]) =
    val mangadexApi         = MangadexApi.make(backend)
    val mangadexScraper     = MangadexScraper[F](mangadexApi)
    val mangakakalotScraper = MangakakalotScraper[F]()

    val pickSiteScraper: Site => SiteScraper[F] =
      case Site.Mangadex     => mangadexScraper
      case Site.Mangakakalot => mangakakalotScraper

    pickSiteScraper
