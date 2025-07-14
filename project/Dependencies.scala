import sbt.*

object Dependencies {
  val catsCore     = "org.typelevel"     %% "cats-core"         % "2.13.0"
  val catsEffect   = "org.typelevel"     %% "cats-effect"       % "3.6.2"
  val catsTime     = "org.typelevel"     %% "cats-time"         % "0.6.0"
  val circeCore    = "io.circe"          %% "circe-core"        % "0.14.14"
  val monocle      = "dev.optics"        %% "monocle-core"      % "3.3.0"
  val ducktape     = "io.github.arainko" %% "ducktape"          % "0.2.9"
  val validator    = "commons-validator"  % "commons-validator" % "1.10.0"
  val scalaScraper = "net.ruippeixotog"  %% "scala-scraper"     % "3.2.0"
  val playwright = "com.microsoft.playwright" % "playwright" % "1.53.0"

  // HTTP Client
  val sttp      = "com.softwaremill.sttp.client3" %% "core"  % "3.11.0"
  val sttpCats  = "com.softwaremill.sttp.client3" %% "cats"  % sttp.revision
  val sttpCirce = "com.softwaremill.sttp.client3" %% "circe" % sttp.revision

  // HTTP Layer
  val http4sServer = "org.http4s" %% "http4s-ember-server" % "0.23.30"
  val http4sDsl   = "org.http4s"  %% "http4s-dsl"   % http4sServer.revision
  val http4sCirce = "org.http4s"  %% "http4s-circe" % http4sServer.revision
  val scalaTags   = "com.lihaoyi" %% "scalatags"    % "0.13.1"

  // DB Layer
  val doobieCore   = "org.tpolecat" %% "doobie-core"   % "1.0.0-RC8"
  val doobieHikari = "org.tpolecat" %% "doobie-hikari" % doobieCore.revision
  val hikariCp     = "com.zaxxer"    % "HikariCP"      % "5.1.0"
  val sqliteJDBC   = "org.xerial"    % "sqlite-jdbc"   % "3.36.0"

  // Logging
  val slf4j      = "org.slf4j" % "slf4j-simple" % "2.0.17" // Only for http4s
  val scribe     = "com.outr" %% "scribe"       % "3.16.1"
  val scribeCats = "com.outr" %% "scribe-cats"  % scribe.revision

  // Tests
  val munit           = "org.scalameta" %% "munit"             % "1.1.1"
  val munitCatsEffect = "org.typelevel" %% "munit-cats-effect" % "2.1.0"
}
