ThisBuild / scalaVersion := "3.3.1"

val commonDependencies = Seq(
  Dependencies.catsCore,
  Dependencies.catsEffect,
  Dependencies.catsTime,
  Dependencies.circeCore,
  Dependencies.monocle,
  Dependencies.ducktape,
  Dependencies.doobieCore,
  Dependencies.scalaTags,
  Dependencies.validator,
  Dependencies.munit % Test
)

lazy val core = project
  .in(file("modules/core"))
  .settings(libraryDependencies ++= commonDependencies)

lazy val doobiex = project
  .in(file("modules/doobiex"))
  .settings(
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(core)

lazy val db = project
  .in(file("modules/db"))
  .settings(
    libraryDependencies ++= commonDependencies ++ Seq(
      Dependencies.doobieHikari,
      Dependencies.hikariCp,
      Dependencies.sqliteJDBC
    )
  )
  .dependsOn(core)

lazy val library = project
  .in(file("modules/library"))
  .settings(
    libraryDependencies ++= commonDependencies ++ Seq(
      Dependencies.http4sCirce,
      Dependencies.http4sDsl,
      Dependencies.http4sServer
    )
  )
  .dependsOn(core, doobiex, db)

lazy val scrapeConfigs = project
  .in(file("modules/scrapeConfigs"))
  .settings(libraryDependencies ++= commonDependencies)
  .dependsOn(core, doobiex)

lazy val scraper = project
  .in(file("modules/scraper"))
  .settings(libraryDependencies ++= commonDependencies)
  .dependsOn(core, scrapeConfigs)

lazy val assetScraping = project
  .in(file("modules/assetScraping"))
  .settings(libraryDependencies ++= commonDependencies)
  .dependsOn(core, library, scrapeConfigs, scraper)

lazy val root = project
  .in(file("."))
  .settings(
    name    := "toshokan",
    version := "0.1.0",
    fork    := true,
    libraryDependencies ++= commonDependencies ++ Seq(Dependencies.slf4j)
  )
  .aggregate(core, library, scraper, scrapeConfigs, assetScraping)
  .dependsOn(core, library, scraper, scrapeConfigs, assetScraping)
