package scraper

import java.net.URI

import cats.effect.{IO, IOApp}
import scraper.util.playwright
import scraper.sites.mangakakalot.MangakakalotScraper

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    playwright
      .makePlaywrightResource[IO]
      .evalMap(p => IO.delay(p.chromium().launch()))
      .use: browser =>
        val scraper = MangakakalotScraper[IO]
        scraper
          .findEntries(new URI("https://chapmanganato.to/manga-du980903"))
          .map:
            case Left(error) => println(error)
            case Right(entries) => 
              val head = entries.head
              println(s"${head.title} ; ${head.no} ; ${head.dateUploaded}")
