package app

import assetScraping.downloading.domain.DownloadDir
import cats.effect.*
import cats.effect.std.Random
import com.microsoft.playwright.Browser
import doobie.Transactor
import http.Routed
import http.View.NavBarItem
import myAnimeList.*
import org.http4s.HttpRoutes
import org.http4s.syntax.all.*
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend

import middleware.logErrors

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    for
      _ <- logging.init[IO]
      (
        serverConfig,
        dbConfig,
        snapshotConfig,
        malAuth,
        downloadDir,
        navBarItems,
        useDnsOverHttps
      ) <- config
        .load[IO]
      random <- Random.scalaUtilRandom[IO]
      result <- app.wiring.CrossCuttingConcernsModule
        .setupResources(dbConfig, useDnsOverHttps)
        .use: resources =>
          val snapshotManager = snapshotConfig
            .map(snapshot.git.GitSnapshotManager[IO](_))
            .getOrElse(snapshot.NoopSnapshotManager[IO])
          createControllers(
            resources.xa,
            resources.httpBackend,
            resources.browser,
            malAuth,
            downloadDir,
            resources.shutdownSignal,
            navBarItems,
            random
          ).flatMap(routes =>
            snapshotManager.saveIfDue() *>
              startServer(
                serverConfig,
                Routed.combine(routes),
                resources.shutdownSignal
              )
          )
    yield result

  private def createControllers(
      xa: Transactor[IO],
      httpBackend: SttpBackend[IO, WebSockets],
      browser: Browser,
      malAuth: Option[MalAuth],
      downloadDir: DownloadDir,
      shutdownSignal: Deferred[IO, Unit],
      navBarItems: List[NavBarItem],
      random: Random[IO]
  ): IO[List[http.Routed[IO]]] =
    for
      library <- IO.pure(app.wiring.LibraryModule.make(xa, navBarItems))
      externals <- IO.pure(
        app.wiring.ExternalServices.make(httpBackend, malAuth, random)
      )
      scraping <- IO.pure(
        app.wiring.AssetScrapingModule.make(
          library,
          externals,
          httpBackend,
          browser,
          downloadDir,
          navBarItems,
          xa
        )
      )
      mal <- app.wiring.MyAnimeListModule.make(externals, xa)
      mapping <- IO.pure(
        app.wiring.AssetMappingModule.make(library, mal, xa, navBarItems)
      )
      progress <- IO.pure(
        app.wiring.ProgressTrackingModule
          .make(library, mal, mapping, navBarItems, xa)
      )
      importing <- IO.pure(
        app.wiring.AssetImportingModule
          .make(library, scraping, mapping, externals, navBarItems)
      )
      system <- IO.pure(app.wiring.SystemModule.make(shutdownSignal))
    yield List(
      library.assetController,
      scraping.scrapingController,
      scraping.configController,
      scraping.downloadingController,
      scraping.scheduleController,
      mal.controller,
      mapping.controller,
      importing.controller,
      progress.controller,
      system.publicController,
      system.shutdownController
    )

  private def startServer(
      serverConfig: ServerConfig,
      routes: HttpRoutes[IO],
      shutdownSignal: Deferred[IO, Unit]
  ) =
    HttpServer[IO]
      .newEmber(serverConfig, logErrors(routes).orNotFound)
      .use(_ => IO.never.race(shutdownSignal.get).void)
