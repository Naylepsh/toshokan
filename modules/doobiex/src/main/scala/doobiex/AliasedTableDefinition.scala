package doobiex

import doobie.*
import doobie.syntax.SqlInterpolator.SingleFragment
import cats.syntax.all.*

class AliasedTableDefinition[T <: TableDefinition](
    private val rawName: String,
    private val alias: String,
    private val original: T
):
  val name = Fragment.const0(s"${rawName} AS ${alias}")

  def column[A: Read: Put](f: T => Column[A]): Column[A] =
    Column(f(original).rawName, alias.some)

  def apply[A: Read: Put](f: T => Column[A]): Column[A] = column(f)

object AliasedTableDefinition:
  given Conversion[AliasedTableDefinition[?], Fragment]                = _.name
  given Conversion[AliasedTableDefinition[?], SingleFragment[Nothing]] = _.name
