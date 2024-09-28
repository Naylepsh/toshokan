package scraper

import java.net.URI

import cats.effect.{IO, IOApp}
import scraper.sites.hitomi.HitomiScraper
import scraper.util.playwright

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    playwright
      .makePlaywrightResource[IO]
      .evalMap(p => IO.delay(p.chromium().launch()))
      .use: browser =>
        val scraper = HitomiScraper[IO](browser, 10_000)
        scraper
          .findEntries(new URI("https://hitomi.la/artist/chicke%20iii-all.html"))
          .map:
            case Left(error) => println(error)
            case Right(entries) => 
              val head = entries.head
              println(s"${head.title} ; ${head.no}")
