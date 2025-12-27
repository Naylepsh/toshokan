package library.category

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import core.Tuples
import db.fragments.*
import doobie.*
import doobie.implicits.*
import neotype.interop.doobie.given

import domain.*

trait CategoryRepository[F[_]]:
  def add(category: NewCategory): F[ExistingCategory]
  def find(id: CategoryId): F[Option[ExistingCategory]]
  def findAll: F[List[ExistingCategory]]

object CategoryRepository:
  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): CategoryRepository[F] =
    new:
      override def add(category: NewCategory): F[ExistingCategory] =
        insertIntoReturning(
          Categories,
          NonEmptyList.of(_.name_ --> category.name),
          _.*
        )
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

private[library] object Categories extends TableDefinition("categories"):
  val id    = Column[CategoryId]("id")
  val name_ = Column[CategoryName]("name")

  val * = Columns((id, name_))
