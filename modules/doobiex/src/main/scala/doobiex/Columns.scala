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
    

  // format: off
  def apply[A1, A2](t: (Column[A1], Column[A2])): Columns[(A1, A2)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3](t: (Column[A1], Column[A2], Column[A3])): Columns[(A1, A2, A3)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4](t: (Column[A1], Column[A2], Column[A3], Column[A4])): Columns[(A1, A2, A3, A4)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5])): Columns[(A1, A2, A3, A4, A5)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6])): Columns[(A1, A2, A3, A4, A5, A6)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7])): Columns[(A1, A2, A3, A4, A5, A6, A7)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8], Column[A9])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8], Column[A9], Column[A10])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8], Column[A9], Column[A10], Column[A11])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8], Column[A9], Column[A10], Column[A11], Column[A12])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8], Column[A9], Column[A10], Column[A11], Column[A12], Column[A13])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8], Column[A9], Column[A10], Column[A11], Column[A12], Column[A13], Column[A14])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8], Column[A9], Column[A10], Column[A11], Column[A12], Column[A13], Column[A14], Column[A15])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8], Column[A9], Column[A10], Column[A11], Column[A12], Column[A13], Column[A14], Column[A15], Column[A16])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8], Column[A9], Column[A10], Column[A11], Column[A12], Column[A13], Column[A14], Column[A15], Column[A16], Column[A17])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8], Column[A9], Column[A10], Column[A11], Column[A12], Column[A13], Column[A14], Column[A15], Column[A16], Column[A17], Column[A18])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    new Columns(makeFragmentUnsafe(t))
  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](t: (Column[A1], Column[A2], Column[A3], Column[A4], Column[A5], Column[A6], Column[A7], Column[A8], Column[A9], Column[A10], Column[A11], Column[A12], Column[A13], Column[A14], Column[A15], Column[A16], Column[A17], Column[A18], Column[A19])): Columns[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    new Columns(makeFragmentUnsafe(t))
  // format: on
