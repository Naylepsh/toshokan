package doobiex

import doobie.*
import doobie.syntax.SqlInterpolator.SingleFragment
import doobie.syntax.string.*
import cats.syntax.all.*

class AliasedTableDefinition[T <: TableDefinition](
    private val rawName: String,
    private val alias: String,
    private val original: T
):
  val name = Fragment.const0(s"${rawName} AS ${alias}")

  def column[A: Read: Write](f: T => Column[A]): Column[A] =
    Column(f(original).rawName, alias.some)

  def apply[A: Read: Write](f: T => Column[A]): Column[A] = column(f)

  def apply[A](f: T => Columns[A]): Columns[A] =
    Columns(
      f(original).sql.rawSql
        .split(",")
        .map(_.trim)
        .map: col =>
          Fragment.const0(s"${alias}.${col}")
        .reduce(_ ++ fr"," ++ _)
    )

object AliasedTableDefinition:
  given Conversion[AliasedTableDefinition[?], Fragment]                = _.name
  given Conversion[AliasedTableDefinition[?], SingleFragment[Nothing]] = _.name
