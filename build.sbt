ThisBuild / scalaVersion := "3.3.1"

val commonSettings = List(
  libraryDependencies ++= Seq(
    Dependencies.catsCore,
    Dependencies.circeCore,
    Dependencies.monocle,
    Dependencies.magnum,
    Dependencies.hikariCp,
    Dependencies.sqliteJDBC,
    Dependencies.munit % Test
  )
)

lazy val core = project
  .in(file("modules/core"))
  .settings(commonSettings: _*)

lazy val library = project
  .in(file("modules/library"))
  .settings(commonSettings: _*)
  .dependsOn(core)

lazy val scraper = project
  .in(file("modules/scaper"))
  .settings(commonSettings: _*)
  .dependsOn(core)

lazy val scrapeConfigs = project
  .in(file("modules/scrapeConfigs"))
  .settings(commonSettings: _*)
  .dependsOn(core)

lazy val root = project
  .in(file("."))
  .settings(
    name    := "toshokan",
    version := "0.1.0",
    fork    := true,
    commonSettings
  )
  .aggregate(core, library, scraper, scrapeConfigs)
  .dependsOn(core, library, scraper, scrapeConfigs)
