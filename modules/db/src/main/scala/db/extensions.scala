package db
package extensions

import doobie.*
import doobie.syntax.string.*
import cats.Reducible
import cats.Functor

extension [T <: TableDefinition](self: AliasedTableDefinition[T])
  def apply[A: Read](f: T => Columns[A]): Columns[A] =
    Columns(
      f(self.original).sql.rawSql
        .split(",")
        .map(_.trim)
        .map: col =>
          Fragment.const0(s"${self.alias}.${col}")
        .reduce(_ ++ fr"," ++ _)
        .typed[A]
    )

extension [A <: TableDefinition](self: A)
  def insertIntoX[F[_]: Reducible: Functor](
      columns: F[A => (Fragment, Fragment)]
  ): Fragment = db.fragments.insertInto(self, columns)

  def insertIntoReturning[F[_]: Reducible: Functor, B](
      insertColumns: F[A => (Fragment, Fragment)],
      returningColumns: A => Columns[B]
  ): Fragment =
    db.fragments.insertIntoReturning(self, insertColumns, returningColumns)

  def updateTableX[F[_]: Reducible: Functor](
      columns: F[A => (Fragment, Fragment)]
  ): Fragment = db.fragments.updateTable(self, columns)
