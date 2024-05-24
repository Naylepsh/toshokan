package library.category

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import core.Tuples
import doobie.*
import doobie.implicits.*
import doobiex.*

import domain.*

trait CategoryRepository[F[_]]:
  def add(category: NewCategory): F[ExistingCategory]
  def find(id: CategoryId): F[Option[ExistingCategory]]
  def findAll: F[List[ExistingCategory]]

object CategoryRepository:
  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): CategoryRepository[F] =
    new:
      override def add(category: NewCategory): F[ExistingCategory] =
        sql"""
          INSERT INTO ${Categories}(${Categories.name_}) 
          VALUES (${category.name}) 
          RETURNING ${Categories.*}
          """
          .queryOf(Categories.*)
          .unique
          .transact(xa)
          .map: row =>
            Tuples.from[ExistingCategory](row)

      override def find(id: CategoryId): F[Option[ExistingCategory]] =
        sql"""
        SELECT ${Categories.*}
        FROM ${Categories}
        WHERE ${Categories.id === id}
        """
          .queryOf(Categories.*)
          .option
          .transact(xa)
          .map: row =>
            row.map(Tuples.from[ExistingCategory](_))

      override def findAll: F[List[ExistingCategory]] =
        sql"""
        SELECT ${Categories.*}
        FROM ${Categories}
        """
          .queryOf(Categories.*)
          .to[List]
          .transact(xa)
          .map: rows =>
            rows.map(Tuples.from[ExistingCategory](_))

private object Categories extends TableDefinition("categories"):
  val id    = Column[CategoryId]("id")
  val name_ = Column[CategoryName]("name")

  val * = Columns((id, name_))
