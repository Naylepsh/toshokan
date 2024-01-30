import sbt._

object Dependencies {
  val catsCore   = "org.typelevel"   %% "cats-core"    % "2.10.0"
  val circeCore  = "io.circe"        %% "circe-core"   % "0.14.6"
  val monocle    = "dev.optics"      %% "monocle-core" % "3.2.0"
  val magnum     = "com.augustnagro" %% "magnum"       % "1.1.0"
  val hikariCp   = "com.zaxxer"       % "HikariCP"     % "5.0.1"
  val sqliteJDBC = "org.xerial"       % "sqlite-jdbc"  % "3.44.0.0"
  val munit      = "org.scalameta"   %% "munit"        % "0.7.29"
}
