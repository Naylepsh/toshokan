package progressTracking
package domain

import cats.syntax.all.*
import core.Newtype
import library.category.domain.CategoryName
import library.domain.{AssetId, EntryNo}

type MangaId = MangaId.Type
object MangaId extends Newtype[Long]:
  def apply(assetId: AssetId, categoryName: CategoryName): Option[MangaId] =
    Option.when(categoryName.toLowerCase().eqv("manga"))(MangaId(assetId.value))

type ExternalMangaId = ExternalMangaId.Type
object ExternalMangaId extends Newtype[Long]

type LatestChapter = LatestChapter.Type
object LatestChapter extends Newtype[Int]:
  def apply(no: EntryNo): Option[LatestChapter] =
    no.value.toDoubleOption.map(d => LatestChapter(d.toInt))

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

type MalMangaMappingId = MalMangaMappingId.Type
object MalMangaMappingId extends Newtype[Long]

case class ExistingMalMangaMapping(
    id: MalMangaMappingId,
    internalId: MangaId,
    externalId: ExternalMangaId
)
