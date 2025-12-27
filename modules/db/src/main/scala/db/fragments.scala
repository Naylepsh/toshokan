package db
package fragments

import cats.syntax.all.*
import cats.{Reducible, Functor}
import doobie.*
import doobie.implicits.*

def insertIntoReturning[F[_]: Reducible: Functor, A <: TableDefinition, B](
    table: A,
    insertColumns: F[A => (Fragment, Fragment)],
    returningColumns: A => Columns[B]
): Fragment =
  sql"${insertInto(table, insertColumns)} RETURNING ${returningColumns(table)}"

def insertInto[F[_]: Reducible: Functor, A <: TableDefinition](
    table: A,
    columns: F[A => (Fragment, Fragment)]
): Fragment =
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

def updateTable[F[_]: Reducible: Functor, A <: TableDefinition](
    table: A,
    columns: F[A => (Fragment, Fragment)]
): Fragment =
  val set = columns
    .map(_(table))
    .mapIntercalate(fr0"", fr0", "):
      case (acc, (name, value)) =>
        fr0"$acc$name=$value"

  sql"UPDATE $table SET $set"
