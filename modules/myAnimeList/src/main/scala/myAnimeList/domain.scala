package myAnimeList
package domain

import cats.syntax.all.*
import core.Newtype

type ExternalMangaId = ExternalMangaId.Type
object ExternalMangaId extends Newtype[Long]

type LatestChapter = LatestChapter.Type
object LatestChapter extends Newtype[Int]

type MangaTitle = MangaTitle.Type
object MangaTitle extends Newtype[String]

enum Term:
  case Name(value: String)
  case Id private (value: ExternalMangaId)

object Term:
  def apply(value: String): Term =
    Term.Id(value).getOrElse(Term.Name(value))

  object Id:
    def apply(value: String): Either[String, Term.Id] =
      value match
        case s"#${id}" =>
          id.toLongOption
            .map(id => Term.Id(ExternalMangaId(id)).asRight)
            .getOrElse(s"'${value}' is not a valid id term".asLeft)
        case _ => s"'${value}' is not a valid id term".asLeft

case class Manga(id: ExternalMangaId, title: MangaTitle)
