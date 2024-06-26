package app
package config

import cats.effect.kernel.Sync
import cats.syntax.all.*
import http.View.NavBarItem

def load[F[_]](using F: Sync[F]) =
  F.delay:
    val databaseUrl              = sys.env("DATABASE_URL")
    val snapshotPath             = sys.env.get("SNAPSHOT_PATH")
    val snapshotRecencyThreshold = sys.env.get("SNAPSHOT_RECENCY_THRESHOLD")

    val serverConfig = ServerConfig.default
    val dbConfig     = db.Config.forSqlite(db.Path(databaseUrl))
    val snapshotConfig = (snapshotPath, snapshotRecencyThreshold).tupled.map:
      (path, threshold) =>
        snapshot.git.Config(
          db.Path(databaseUrl),
          snapshot.git.PathToSnapshot(path),
          snapshot.git.RecencyThreshold(threshold.toInt)
        )
    val navBarItems = List(
      NavBarItem("Assets", "/assets"),
      NavBarItem("New releases", "/assets/entries-by-release-date"),
      NavBarItem("Scraping", "/asset-scraping"),
      NavBarItem("Shutdown", "/shutdown")
    )

    (
      serverConfig,
      dbConfig,
      snapshotConfig,
      navBarItems
    )
