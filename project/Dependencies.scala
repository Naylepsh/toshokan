import sbt._

object Dependencies {
  val catsCore     = "org.typelevel"     %% "cats-core"         % "2.10.0"
  val catsEffect   = "org.typelevel"     %% "cats-effect"       % "3.5.3"
  val catsTime     = "org.typelevel"     %% "cats-time"         % "0.5.1"
  val circeCore    = "io.circe"          %% "circe-core"        % "0.14.6"
  val monocle      = "dev.optics"        %% "monocle-core"      % "3.2.0"
  val ducktape     = "io.github.arainko" %% "ducktape"          % "0.1.11"
  val validator    = "commons-validator"  % "commons-validator" % "1.8.0"
  val scalaScraper = "net.ruippeixotog"  %% "scala-scraper"     % "3.1.1"

  // HTTP Client
  val sttp      = "com.softwaremill.sttp.client3" %% "core"  % "3.9.3"
  val sttpCats  = "com.softwaremill.sttp.client3" %% "cats"  % sttp.revision
  val sttpCirce = "com.softwaremill.sttp.client3" %% "circe" % sttp.revision

  // HTTP Layer
  val http4sServer = "org.http4s"  %% "http4s-ember-server" % "0.23.25"
  val http4sDsl    = "org.http4s"  %% "http4s-dsl"          % http4sServer.revision
  val http4sCirce  = "org.http4s"  %% "http4s-circe"        % http4sServer.revision
  val scalaTags    = "com.lihaoyi" %% "scalatags"           % "0.12.0"

  // DB Layer
  val doobieCore   = "org.tpolecat" %% "doobie-core"   % "1.0.0-RC5"
  val doobieHikari = "org.tpolecat" %% "doobie-hikari" % doobieCore.revision
  val hikariCp     = "com.zaxxer"    % "HikariCP"      % "5.0.1"
  val sqliteJDBC   = "org.xerial"    % "sqlite-jdbc"   % "3.44.0.0"

  // Logging
  val slf4j      = "org.slf4j" % "slf4j-simple" % "2.0.7" // Only for http4s
  val scribe     = "com.outr" %% "scribe"       % "3.13.2"
  val scribeCats = "com.outr" %% "scribe-cats"  % scribe.revision

  // Tests
  val munit = "org.scalameta" %% "munit" % "0.7.29"
}
