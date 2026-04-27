package scraper.util.playwright

import cats.effect.{IO, Resource}
import com.microsoft.playwright.*
import com.microsoft.playwright.Page.WaitForSelectorOptions

extension (page: Page)
  def navigateSafe(url: String): IO[Response] =
    IO.delay(page.navigate(url))

  def waitForSelectorSafe(
      selector: String,
      options: WaitForSelectorOptions
  ): IO[Either[Throwable, ElementHandle]] =
    IO.delay(page.waitForSelector(selector, options)).attempt

extension (browser: Browser)
  def makePage: Resource[IO, Page] =
    Resource.make(IO.delay(browser.newPage()))(p => IO.delay(p.close()))

def makePlaywrightResource: Resource[IO, Playwright] =
  Resource.make(IO.delay(Playwright.create()))(pw => IO.delay(pw.close()))
