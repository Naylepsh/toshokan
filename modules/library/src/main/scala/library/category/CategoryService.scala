package library.category

import cats.effect.kernel.MonadCancelThrow
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import neotype.interop.cats.given

import domain.*

trait CategoryService[F[_]]:
  def find(id: CategoryId): F[Option[ExistingCategory]]
  def find(ids: List[CategoryId]): F[List[ExistingCategory]]
  def findAll: F[List[ExistingCategory]]
  def findManga: F[Option[ExistingCategory]]
  def add(
      newCategory: NewCategory
  ): Raise[F, CategoryAlreadyExists] ?=> F[ExistingCategory]

object CategoryService:
  def make[F[_]: MonadCancelThrow](
      repository: CategoryRepository,
      xa: Transactor[F]
  ): CategoryService[F] = new:

    override def find(id: CategoryId): F[Option[ExistingCategory]] =
      repository.find(id).transact(xa)

    override def find(ids: List[CategoryId]): F[List[ExistingCategory]] =
      findAll.map: categories =>
        categories.filter: category =>
          ids.contains(category.id)

    override def findAll: F[List[ExistingCategory]] =
      repository.findAll.transact(xa)

    override def findManga: F[Option[ExistingCategory]] =
      findAll.map(_.find(_.name.eqv(CategoryName("manga"))))

    override def add(
        newCategory: NewCategory
    ): Raise[F, CategoryAlreadyExists] ?=> F[ExistingCategory] =
      repository.findAll
        .transact(xa)
        .flatMap: categories =>
          categories
            .find(_.name.eqv(newCategory.name))
            .fold(repository.add(newCategory).transact(xa)): _ =>
              CategoryAlreadyExists.raise
