import assetScraping.domain.Site
import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import scraper.domain.SiteScraper
import scraper.sites.mangadex.{ MangadexApi, MangadexScraper }
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend

object SiteScrapers:
  def makeScraperPicker[F[_]: MonadCancelThrow](httpClient: Resource[
    F,
    SttpBackend[F, WebSockets]
  ]) =
    val mangadexApi     = MangadexApi.make(httpClient)
    val mangadexScraper = MangadexScraper[F](mangadexApi)

    val pickSiteScraper: Site => SiteScraper[F] =
      case Site.Mangadex     => mangadexScraper
      case Site.Mangakakalot => ???

    pickSiteScraper
