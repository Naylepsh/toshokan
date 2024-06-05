package library.category

import cats.Functor
import cats.syntax.functor.*

import domain.*

trait CategoryService[F[_]]:
  def find(id: CategoryId): F[Option[ExistingCategory]]
  def find(ids: List[CategoryId]): F[List[ExistingCategory]]
  def findAll: F[List[ExistingCategory]]

object CategoryService:
  def make[F[_]: Functor](
      repository: CategoryRepository[F]
  ): CategoryService[F] = new:

    override def find(id: CategoryId): F[Option[ExistingCategory]] =
      repository.find(id)

    override def find(ids: List[CategoryId]): F[List[ExistingCategory]] =
      findAll.map: categories =>
        categories.filter: category =>
          ids.contains(category.id)

    override def findAll: F[List[ExistingCategory]] = repository.findAll
