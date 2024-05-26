package library.category

import domain.*

trait CategoryService[F[_]]:
  def find(id: CategoryId): F[Option[ExistingCategory]]
  def findAll: F[List[ExistingCategory]]

object CategoryService:
  def make[F[_]](repository: CategoryRepository[F]): CategoryService[F] = new:

    override def find(id: CategoryId): F[Option[ExistingCategory]] =
      repository.find(id)

    override def findAll: F[List[ExistingCategory]] = repository.findAll
