package doobiex

import cats.syntax.all.*
import cats.{Functor, Reducible}
import doobie.*
import doobie.implicits.*
import doobie.util.pos.Pos
import doobie.util.fragment.Fragment

def insertInto[F[_]: Reducible: Functor, A <: TableDefinition](
    table: A,
    columns: F[A => (Fragment, Fragment)]
)(using Pos): Fragment =
  val columnNames = columns
    .map(_(table))
    .mapIntercalate(fr0"", fr0", "):
      case (acc, (name, _)) =>
        fr0"$acc$name"

  val values = columns
    .map(_(table))
    .mapIntercalate(fr0"", fr0", "):
      case (acc, (_, value)) =>
        fr0"$acc$value"

  sql"INSERT INTO $table ($columnNames) VALUES ($values)"

def insertIntoReturning[F[_]: Reducible: Functor, A <: TableDefinition, B](
    table: A,
    insertColumns: F[A => (Fragment, Fragment)],
    returningColumns: A => Columns[B]
): Fragment =
  sql"${insertInto(table, insertColumns)} RETURNING ${returningColumns(table)}"
