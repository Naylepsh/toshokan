package scraper

import cats.effect.IOApp
import cats.effect.IO
import scraper.sites.mangakakalot.MangakakalotScraper
import java.net.URI

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    val scraper = MangakakalotScraper[IO]()
    scraper
      .findEntries(new URI("https://chapmanganato.to/manga-qq993573"))
      .flatMap(IO.println)
