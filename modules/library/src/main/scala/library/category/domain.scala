package library.category.domain

import scala.util.control.NoStackTrace

type CategoryId = CategoryId.Type
object CategoryId extends neotype.Newtype[Long]

type CategoryName = CategoryName.Type
object CategoryName extends neotype.Subtype[String]

case class NewCategory(name: CategoryName)
case class ExistingCategory(id: CategoryId, name: CategoryName)

case object CategoryDoesNotExist extends NoStackTrace
type CategoryDoesNotExist = CategoryDoesNotExist.type

case object CategoryAlreadyExists extends NoStackTrace
type CategoryAlreadyExists = CategoryAlreadyExists.type
