package app

import assetMapping.MalMangaMappingRepository
import assetScraping.configs.{
  AssetScrapingConfigRepository,
  AuthorScrapingConfigRepository
}
import assetScraping.downloading.domain.DownloadDir
import authorMerging.{AuthorMergeController, AuthorMergeService}
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
      _ <- logging.init
      (
        serverConfig,
        dbConfig,
        snapshotConfig,
        malAuth,
        downloadDir,
        navBarItems,
        useDnsOverHttps
      )      <- config.load
      random <- Random.scalaUtilRandom[IO]
      result <- app.wiring.CrossCuttingConcernsModule
        .setupResources(dbConfig, useDnsOverHttps)
        .use: resources =>
          val snapshotManager = snapshotConfig
            .map(snapshot.git.GitSnapshotManager(_))
            .getOrElse(snapshot.NoopSnapshotManager())
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
  ): IO[List[http.Routed]] =
    val library = app.wiring.LibraryModule.make(xa, navBarItems)
    val authorMergeService = AuthorMergeService(
      library.authorRepository,
      library.assetRepository,
      AuthorScrapingConfigRepository.make,
      AssetScrapingConfigRepository.make,
      MalMangaMappingRepository.make,
      xa
    )
    val authorMergeController = AuthorMergeController(
      authorMergeService,
      library.authorService,
      library.authorView
    )
    val externals =
      app.wiring.ExternalServices.make(httpBackend, malAuth, random)
    val scraping = app.wiring.AssetScrapingModule.make(
      library,
      externals,
      httpBackend,
      browser,
      downloadDir,
      navBarItems,
      xa
    )

    for mal <- app.wiring.MyAnimeListModule.make(externals, xa)
    yield
      val mapping =
        app.wiring.AssetMappingModule.make(library, mal, xa, navBarItems)
      val progress = app.wiring.ProgressTrackingModule.make(
        library,
        mal,
        mapping,
        navBarItems,
        xa
      )
      val importing = app.wiring.AssetImportingModule.make(
        library,
        scraping,
        mapping,
        externals,
        navBarItems,
        xa
      )
      val system = app.wiring.SystemModule.make(shutdownSignal)

      List(
        library.assetController,
        library.authorController,
        authorMergeController,
        scraping.scrapingController,
        scraping.configController,
        scraping.authorConfigController,
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
    HttpServer
      .newEmber(serverConfig, logErrors(routes).orNotFound)
      .use(_ => IO.never.race(shutdownSignal.get).void)
