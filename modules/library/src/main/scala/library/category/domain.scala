package library.category.domain

import core.Newtype

type CategoryId = CategoryId.Type
object CategoryId extends Newtype[Long]

type CategoryName = CategoryName.Type
object CategoryName extends Newtype[String]

case class NewCategory(name: CategoryName)
case class ExistingCategory(id: CategoryId, name: CategoryName)

enum AddCategoryError:
  case CategoryAlreadyExists
