package doobiex

import doobie.*
import doobie.implicits.*
import doobie.syntax.SqlInterpolator.SingleFragment
import cats.syntax.all.*

class TableDefinition(
    private val rawName: String,
    private val alias: Option[String] = None
):
  val name =
    Fragment.const0(alias.fold(rawName)(alias => s"${alias}.${rawName}"))

  def as(alias: String): AliasedTableDefinition[this.type] =
    AliasedTableDefinition(rawName, alias, this)

object TableDefinition:
  given Conversion[TableDefinition, Fragment] = table => fr"${table.name}"
  given Conversion[TableDefinition, SingleFragment[Nothing]] =
    table => fr"${table.name}"
