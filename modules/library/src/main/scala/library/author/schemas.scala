package library.author
package schemas

import domain.AuthorId

object AuthorIdVar:
  def unapply(str: String): Option[AuthorId] =
    str.toLongOption.map(AuthorId(_))
