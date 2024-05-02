package db

import core.Newtype

type Path = Path.Type
object Path extends Newtype[String]:
  extension (path: Path)
    def withoutProtocol: Path = Path(path.replaceFirst(".*://", ""))

type Username = Username.Type
object Username extends Newtype[String]

type Password = Password.Type
object Password extends Newtype[String]

case class Config(
    path: Path,
    username: Username,
    password: Password
)
object Config:
  def forSqlite(path: Path): Config = Config(path, Username(""), Password(""))
