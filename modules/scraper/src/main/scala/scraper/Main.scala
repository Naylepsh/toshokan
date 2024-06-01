package scraper

import java.net.URI

import cats.effect.{IO, IOApp}
import scraper.sites.mangakakalot.MangakakalotScraper

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    val scraper = MangakakalotScraper[IO]()
    scraper
      .findEntries(new URI("https://chapmanganato.to/manga-qq993573"))
      .flatMap(IO.println)
