package app
package config

import java.net.URI
import java.nio.file.Path

import assetScraping.downloading.domain.DownloadDir
import cats.effect.kernel.Sync
import cats.syntax.all.*
import http.View.NavBarItem
import myAnimeList.MalAuth

def load[F[_]](using F: Sync[F]) =
  F.delay:
    val databaseUrl              = sys.env("DATABASE_URL")
    val snapshotPath             = sys.env.get("SNAPSHOT_PATH")
    val snapshotRecencyThreshold = sys.env.get("SNAPSHOT_RECENCY_THRESHOLD")
    val malClientId              = sys.env.get("MAL_CLIENT_ID")
    val malSecret                = sys.env.get("MAL_SECRET")
    val malRedirectUrl  = sys.env.get("MAL_REDIRECT_URL").map(new URI(_))
    val downloadDir     = DownloadDir(Path.of(sys.env("DOWNLOAD_DIR")))
    val useDnsOverHttps = sys.env.get("USE_DNS_OVER_HTTPS").contains("true")

    val serverConfig = ServerConfig.default
    val dbConfig     = db.Config.forSqlite(db.Path(databaseUrl))
    val snapshotConfig = (snapshotPath, snapshotRecencyThreshold).tupled.map:
      (path, threshold) =>
        snapshot.git.Config(
          db.Path(databaseUrl),
          snapshot.git.PathToSnapshot(path),
          snapshot.git.RecencyThreshold(threshold.toInt)
        )
    val malAuth =
      (malClientId, malSecret, malRedirectUrl).tupled.map(MalAuth.apply)
    val navBarItems = List(
      NavBarItem("Assets", "/assets"),
      NavBarItem("Stale assets", "/assets/stale"),
      NavBarItem("New releases", "/progress-tracking/releases"),
      NavBarItem("Scraping", "/asset-scraping"),
      NavBarItem("Shutdown", "/shutdown")
    )

    (
      serverConfig,
      dbConfig,
      snapshotConfig,
      malAuth,
      downloadDir,
      navBarItems,
      useDnsOverHttps
    )
