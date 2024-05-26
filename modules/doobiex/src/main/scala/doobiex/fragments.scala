package doobiex

import doobie.*
import cats.Reducible
import doobie.implicits.*
import doobie.util.pos.Pos

def insertInto[F[_]](table: TableDefinition, columns: F[(Fragment, Fragment)])(
    using
    Reducible[F],
    Pos
): Fragment =
  val columnNames = columns.mapIntercalate(fr0"", fr0", ") {
    case (acc, (name, _)) =>
      fr0"$acc$name"
  }
  val values = columns.mapIntercalate(fr0"", fr0", ") {
    case (acc, (_, value)) =>
      fr0"$acc$value"
  }
  sql"INSERT INTO $table ($columnNames) VALUES ($values)"
