package library.category.domain

import scala.util.control.NoStackTrace

import cats.syntax.all.*
import library.asset.domain.AssetId
import neotype.*

type CategoryId = CategoryId.Type
object CategoryId extends neotype.Newtype[Long]

type CategoryName = CategoryName.Type
object CategoryName extends neotype.Subtype[String]

type MangaId = MangaId.Type
object MangaId extends neotype.Newtype[Long]:
  def apply(assetId: AssetId, categoryName: CategoryName): Option[MangaId] =
    Option
      .when(categoryName.toLowerCase().eqv("manga"))(MangaId(assetId.unwrap))

case class NewCategory(name: CategoryName)
case class ExistingCategory(id: CategoryId, name: CategoryName)

case object CategoryDoesNotExist extends NoStackTrace
type CategoryDoesNotExist = CategoryDoesNotExist.type

case object CategoryAlreadyExists extends NoStackTrace
type CategoryAlreadyExists = CategoryAlreadyExists.type
