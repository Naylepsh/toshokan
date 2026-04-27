package library.category

import cats.effect.IO
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import neotype.interop.cats.given

import domain.*

trait CategoryService:
  def find(id: CategoryId): IO[Option[ExistingCategory]]
  def find(ids: List[CategoryId]): IO[List[ExistingCategory]]
  def findAll: IO[List[ExistingCategory]]
  def findManga: IO[Option[ExistingCategory]]
  def add(
      newCategory: NewCategory
  ): Raise[IO, CategoryAlreadyExists] ?=> IO[ExistingCategory]

object CategoryService:
  def make(
      repository: CategoryRepository,
      xa: Transactor[IO]
  ): CategoryService = new:

    override def find(id: CategoryId): IO[Option[ExistingCategory]] =
      repository.find(id).transact(xa)

    override def find(ids: List[CategoryId]): IO[List[ExistingCategory]] =
      findAll.map(_.filter(c => ids.contains(c.id)))

    override def findAll: IO[List[ExistingCategory]] =
      repository.findAll.transact(xa)

    override def findManga: IO[Option[ExistingCategory]] =
      findAll.map(_.find(_.name.eqv(CategoryName("manga"))))

    override def add(
        newCategory: NewCategory
    ): Raise[IO, CategoryAlreadyExists] ?=> IO[ExistingCategory] =
      repository.findAll
        .transact(xa)
        .flatMap: categories =>
          categories
            .find(_.name.eqv(newCategory.name))
            .fold(repository.add(newCategory).transact(xa)): _ =>
              CategoryAlreadyExists.raise
