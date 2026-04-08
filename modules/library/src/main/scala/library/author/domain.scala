package library.author.domain

type AuthorId = AuthorId.Type
object AuthorId extends neotype.Newtype[Long]

type AuthorName = AuthorName.Type
object AuthorName extends neotype.Subtype[String]

case class NewAuthor(name: AuthorName)

case class ExistingAuthor(id: AuthorId, name: AuthorName)
