package db

type Path = Path.Type
object Path extends neotype.Subtype[String]:
  extension (path: Path)
    def withoutProtocol: Path = Path(path.replaceFirst(".*://", ""))

type Username = Username.Type
object Username extends neotype.Subtype[String]

type Password = Password.Type
object Password extends neotype.Subtype[String]

case class Config(
    path: Path,
    username: Username,
    password: Password
)
object Config:
  def forSqlite(path: Path): Config = Config(path, Username(""), Password(""))
