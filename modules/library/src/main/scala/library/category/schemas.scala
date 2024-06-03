package library.category
package schemas

import domain.CategoryId

object CategoryIdVar:
  def unapply(str: String): Option[CategoryId] =
    str.toLongOption.map(CategoryId(_))
