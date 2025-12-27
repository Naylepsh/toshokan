package scraper.util.playwright

import cats.effect.Resource
import cats.effect.kernel.Sync
import cats.syntax.all.*
import com.microsoft.playwright.*
import com.microsoft.playwright.Page.WaitForSelectorOptions

extension (page: Page)
  def navigateSafe[F[_]](url: String)(using F: Sync[F]): F[Response] =
    F.delay(page.navigate(url))

  def waitForSelectorSafe[F[_]](
      selector: String,
      options: WaitForSelectorOptions
  )(using F: Sync[F]): F[Either[Throwable, ElementHandle]] =
    F.delay(page.waitForSelector(selector, options)).attempt

extension (browser: Browser)
  def makePage[F[_]](using F: Sync[F]): Resource[F, Page] =
    Resource.make(F.delay(browser.newPage())): p =>
      F.delay(p.close())

def makePlaywrightResource[F[_]](using F: Sync[F]): Resource[F, Playwright] =
  Resource.make(F.delay(Playwright.create())): playwright =>
    F.delay(playwright.close())
