package doobiex

import doobie.*
import doobie.implicits.*
import doobie.syntax.SqlInterpolator.SingleFragment

extension (fragment: Fragment)
  inline def queryOf[A](col: Column[A])         = fragment.query[A](using col.read)
  inline def queryOf[A: Read](cols: Columns[A]) = fragment.query[A]
