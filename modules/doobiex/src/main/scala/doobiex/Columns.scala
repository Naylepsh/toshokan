package doobiex

import doobie.*
import doobie.implicits.*
import doobie.syntax.SqlInterpolator.SingleFragment

case class Columns[+A](sql: Fragment)

object Columns:
  given Conversion[Columns[?], Fragment]          = _.sql
  given Conversion[Columns[?], SingleFragment[?]] = _.sql

  private def concatFrags(frags: Iterable[Fragment]): Fragment =
    frags
      .headOption
      .map: head =>
        frags.tail.foldLeft(head)(_ ++ fr"," ++ _)
      .getOrElse(fr0"")

  private def makeFragmentUnsafe(t: Tuple): Fragment =
    import Column.*
    val frags =
      Iterable.from(t.productIterator.map(_.asInstanceOf[Column[?]].sql))
    frags
      .headOption
      .map: head =>
        frags.tail.foldLeft(head)(_ ++ fr"," ++ _)
      .getOrElse(fr0"")

  def apply[A1, A2](t: (Column[A1], Column[A2])): Columns[(A1, A2)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3](t: (Column[A1], Column[A2], Column[A3]))
      : Columns[(A1, A2, A3)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4](t: (Column[A1], Column[A2], Column[A3], Column[A4]))
      : Columns[(A1, A2, A3, A4)] =
    new Columns(makeFragmentUnsafe(t))
