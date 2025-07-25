package doobiex

import doobie.*
import doobie.implicits.*
import doobie.syntax.SqlInterpolator.SingleFragment

import scala.util.NotGiven

case class Column[A: Read: Write](
    rawName: String,
    alias: Option[String] = None
):
  self =>
  type T = A

  val name =
    Fragment.const0(alias.fold(rawName)(alias => s"${alias}.${rawName}"))
  val sql = name

  def read  = summon[Read[A]]
  def write = summon[Write[A]]

  /** Returns the [[Get]] that is backing the [[read]]. */
  lazy val get: Get[A] =
    // This is somewhat unsafe, but given that a column is a single column, it should be fine.
    read.gets.head._1.asInstanceOf[Get[A]]

  /** Returns the [[Put]] that is backing the [[write]]. */
  lazy val put: Put[A] =
    // This is somewhat unsafe, but given that a column is a single column, it should be fine.
    write.puts.head._1.asInstanceOf[Put[A]]

  def option[B](using NotGiven[A =:= Option[B]]): Column[Option[A]] =
    given Read[Option[A]]  = Read.fromGetOption(using self.get)
    given Write[Option[A]] = Write.fromPutOption(using self.put)
    Column[Option[A]](rawName, alias)

  def ===(value: Option[A]): Fragment = value match
    case None    => sql ++ fr"is null"
    case Some(v) => ===(v)
  def ===(value: A): Fragment = sql ++ fr" = ${value}"

  def -->(a: A): (Fragment, Fragment) = (name, fr0"$a")

object Column:
  given [A]: Conversion[Column[A], SingleFragment[A]] =
    c => SingleFragment(fr"${c.name}")
  given [A]: Conversion[Column[A], Fragment] = c => fr"${c.name}"
