package doobiex

import doobie.*

import cats.{Foldable, Semigroup}
import cats.syntax.foldable.*

extension [F[_], A](fa: F[A])(using Foldable[F])
  /** Example:
    * {{{
    *   List(1, 2, 3).mapIntercalate("Words: ", ", ")((str, num) => s"$str$num")
    *   // "Words: 1, 2, 3"
    * }}}
    *
    * @param starting
    *   the starting value
    * @param separator
    *   the values that go between the elements
    */
  def mapIntercalate[B](starting: B, separator: B)(
      f: (B, A) => B
  )(using semi: Semigroup[B]): B =
    var first = true
    fa.foldLeft(starting): (b, a) =>
      if first then
        first = false
        f(b, a)
      else f(semi.combine(b, separator), a)

extension (fragment: Fragment)
  inline def queryOf[A](col: Column[A]) = fragment.query[A](using col.read)
  @annotation.nowarn()
  inline def queryOf[A: Read](cols: Columns[A]) = fragment.query[A]
