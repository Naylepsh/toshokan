package library.category

import domain.*
import cats.Monad
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*

trait CategoryService[F[_]]:
  def find(id: CategoryId): F[Option[ExistingCategory]]
  def find(ids: List[CategoryId]): F[List[ExistingCategory]]
  def findAll: F[List[ExistingCategory]]
  def findManga: F[Option[ExistingCategory]]
  def add(
      newCategory: NewCategory
  ): Raise[F, AddCategoryError] ?=> F[ExistingCategory]

object CategoryService:
  def make[F[_]: Monad](
      repository: CategoryRepository[F]
  ): CategoryService[F] = new:

    override def find(id: CategoryId): F[Option[ExistingCategory]] =
      repository.find(id)

    override def find(ids: List[CategoryId]): F[List[ExistingCategory]] =
      findAll.map: categories =>
        categories.filter: category =>
          ids.contains(category.id)

    override def findAll: F[List[ExistingCategory]] = repository.findAll

    override def findManga: F[Option[ExistingCategory]] =
      findAll.map(_.find(_.name.eqv(CategoryName("manga"))))

    override def add(
        newCategory: NewCategory
    ): Raise[F, AddCategoryError] ?=> F[ExistingCategory] =
      repository.findAll.flatMap: categories =>
        categories
          .find(_.name.eqv(newCategory.name))
          .fold(repository.add(newCategory)): _ =>
            AddCategoryError.CategoryAlreadyExists.raise
