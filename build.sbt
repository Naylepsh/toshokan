ThisBuild / scalaVersion      := "3.7.1"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

val commonDependencies = Seq(
  Dependencies.catsCore,
  Dependencies.catsEffect,
  Dependencies.catsTime,
  Dependencies.catsMtl,
  Dependencies.circeCore,
  Dependencies.ducktape,
  Dependencies.doobieCore,
  Dependencies.doobieTypesafe,
  Dependencies.neotype,
  Dependencies.neotypeCirce,
  Dependencies.neotypeDoobie,
  Dependencies.neotypeCats,
  Dependencies.validator,
  Dependencies.scribe,
  Dependencies.scribeCats,
  Dependencies.munit           % Test,
  Dependencies.munitCatsEffect % Test
)

lazy val core = project
  .in(file("modules/core"))
  .disablePlugins(RevolverPlugin)
  .settings(libraryDependencies ++= commonDependencies)

lazy val db = project
  .in(file("modules/db"))
  .disablePlugins(RevolverPlugin)
  .settings(
    libraryDependencies ++= commonDependencies ++ Seq(
      Dependencies.doobieHikari,
      Dependencies.hikariCp,
      Dependencies.sqliteJDBC
    )
  )
  .dependsOn(core)

lazy val http = project
  .in(file("modules/http"))
  .disablePlugins(RevolverPlugin)
  .settings(
    libraryDependencies ++= commonDependencies ++ Seq(
      Dependencies.http4sCirce,
      Dependencies.http4sDsl,
      Dependencies.scalaTags
    )
  )
  .dependsOn(core)

lazy val mangadex = project
  .in(file("modules/mangadex"))
  .disablePlugins(RevolverPlugin)
  .settings(
    libraryDependencies ++= commonDependencies ++ Seq(
      Dependencies.sttp,
      Dependencies.sttpCats,
      Dependencies.sttpCirce
    )
  )
  .dependsOn(core)

lazy val myAnimeList = project
  .in(file("modules/myAnimeList"))
  .disablePlugins(RevolverPlugin)
  .settings(
    libraryDependencies ++= commonDependencies ++ Seq(
      Dependencies.sttp,
      Dependencies.sttpCats,
      Dependencies.sttpCirce,
      Dependencies.http4sDsl,
      Dependencies.http4sServer
    )
  )
  .dependsOn(core, db, http)

lazy val library = project
  .in(file("modules/library"))
  .disablePlugins(RevolverPlugin)
  .settings(
    libraryDependencies ++= commonDependencies ++ Seq(
      Dependencies.http4sCirce,
      Dependencies.http4sDsl,
      Dependencies.http4sServer
    )
  )
  .dependsOn(core, db, http)

lazy val scraper = project
  .in(file("modules/scraper"))
  .disablePlugins(RevolverPlugin)
  .settings(
    libraryDependencies ++= commonDependencies ++ Seq(
      Dependencies.sttp,
      Dependencies.sttpCats,
      Dependencies.sttpCirce,
      Dependencies.scalaScraper,
      Dependencies.playwright
    )
  )
  .dependsOn(core, mangadex)

lazy val assetScraping = project
  .in(file("modules/assetScraping"))
  .disablePlugins(RevolverPlugin)
  .settings(libraryDependencies ++= commonDependencies)
  .dependsOn(core, library, scraper)

lazy val assetMapping = project
  .in(file("modules/assetMapping"))
  .disablePlugins(RevolverPlugin)
  .settings(
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(core, db, library, myAnimeList)

lazy val assetImporting = project
  .in(file("modules/assetImporting"))
  .disablePlugins(RevolverPlugin)
  .settings(
    libraryDependencies ++= commonDependencies ++ Seq(
      Dependencies.http4sCirce,
      Dependencies.http4sDsl,
      Dependencies.http4sServer
    )
  )
  .dependsOn(core, library, assetScraping, assetMapping, mangadex, http)

lazy val snapshot = project
  .in(file("modules/snapshot"))
  .disablePlugins(RevolverPlugin)
  .settings(libraryDependencies ++= commonDependencies)
  .dependsOn(core, db)

lazy val progressTracking = project
  .in(file("modules/progressTracking"))
  .disablePlugins(RevolverPlugin)
  .settings(
    libraryDependencies ++= commonDependencies ++ Seq(
      Dependencies.sttp,
      Dependencies.sttpCats,
      Dependencies.sttpCirce
    )
  )
  .dependsOn(core, db, library, myAnimeList, assetMapping)

lazy val app = project
  .in(file("modules/app"))
  .settings(
    name := "app",
    libraryDependencies ++= commonDependencies ++ Seq(
      Dependencies.slf4j,
      Dependencies.sttpOkHttp,
      Dependencies.okHttpDoH
    )
  )
  .aggregate(
    core,
    library,
    scraper,
    assetScraping,
    assetImporting,
    snapshot,
    progressTracking
  )
  .dependsOn(
    core,
    library,
    scraper,
    assetScraping,
    assetImporting,
    snapshot,
    progressTracking
  )
  .disablePlugins(RevolverPlugin)

lazy val root = project
  .in(file("."))
  .settings(
    name                := "toshokan",
    version             := "0.1.0",
    fork                := true,
    Compile / mainClass := Some("app.Main"),
    Compile / run       := Some("app.Main"),
    Universal / run     := Some("app.Main")
  )
  .aggregate(app)
  .dependsOn(app)
  .enablePlugins(JavaAppPackaging) // SBT native packager

addCommandAlias("fmt", "scalafixAll OrganizeImports; scalafmtAll; scalafmtSbt")
