package scraper

import java.net.URI

import cats.effect.{IO, IOApp}
import scraper.sites.empik.EmpikScraper

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    val scraper = EmpikScraper[IO]
    scraper
      .findEntries(
        new URI(
          "https://www.empik.com/ksiazki/komiks/manga,317405,s?q=moja%20gwiazda"
        )
      )
      .map:
        case Left(error) => println(error)
        case Right(entries) =>
          val head = entries.head
          println(s"${head.title} ; ${head.no} ; ${head.dateUploaded}")
