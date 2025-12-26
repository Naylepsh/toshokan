package library.category.domain

type CategoryId = CategoryId.Type
object CategoryId extends neotype.Newtype[Long]

type CategoryName = CategoryName.Type
object CategoryName extends neotype.Subtype[String]

case class NewCategory(name: CategoryName)
case class ExistingCategory(id: CategoryId, name: CategoryName)

enum AddCategoryError:
  case CategoryAlreadyExists
