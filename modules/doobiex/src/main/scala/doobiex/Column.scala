package doobiex

import doobie.*
import doobie.implicits.*
import doobie.syntax.SqlInterpolator.SingleFragment

case class Column[A: Read: Put](rawName: String):
  val name = Fragment.const0(f"${rawName}")
  val sql  = name

  def read = summon[Read[A]]

object Column:
  given [A]: Conversion[Column[A], SingleFragment[A]] =
    c => SingleFragment(fr"${c.name}")
  given [A]: Conversion[Column[A], Fragment] = c => fr"${c.name}"
