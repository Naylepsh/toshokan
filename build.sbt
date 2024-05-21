ThisBuild / scalaVersion := "3.4.0"

val commonDependencies = Seq(
  Dependencies.catsCore,
  Dependencies.catsEffect,
  Dependencies.catsTime,
  Dependencies.circeCore,
  Dependencies.monocle,
  Dependencies.ducktape,
  Dependencies.doobieCore,
  Dependencies.validator,
  Dependencies.scribe,
  Dependencies.scribeCats,
  Dependencies.munit % Test
)

lazy val core = project
  .in(file("modules/core"))
  .disablePlugins(RevolverPlugin)
  .settings(libraryDependencies ++= commonDependencies)

lazy val doobiex = project
  .in(file("modules/doobiex"))
  .disablePlugins(RevolverPlugin)
  .settings(
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(core)

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
  .dependsOn(core, doobiex, db, http)

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
  .dependsOn(core)

lazy val assetScraping = project
  .in(file("modules/assetScraping"))
  .disablePlugins(RevolverPlugin)
  .settings(libraryDependencies ++= commonDependencies)
  .dependsOn(core, library, scraper)

lazy val snapshot = project
  .in(file("modules/snapshot"))
  .disablePlugins(RevolverPlugin)
  .settings(libraryDependencies ++= commonDependencies)
  .dependsOn(core, db)

lazy val app = project
  .in(file("modules/app"))
  .settings(
    name := "app",
    libraryDependencies ++= commonDependencies ++ Seq(Dependencies.slf4j)
  )
  .aggregate(core, library, scraper, assetScraping, snapshot)
  .dependsOn(core, library, scraper, assetScraping, snapshot)
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
