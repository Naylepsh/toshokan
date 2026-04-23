package library.category

import cats.data.NonEmptyList
import core.Tuples
import db.fragments.*
import doobie.*
import doobie.implicits.*
import neotype.interop.doobie.given

import domain.*

trait CategoryRepository:
  def add(category: NewCategory): ConnectionIO[ExistingCategory]
  def find(id: CategoryId): ConnectionIO[Option[ExistingCategory]]
  def findAll: ConnectionIO[List[ExistingCategory]]

object CategoryRepository:
  val make: CategoryRepository =
    new:
      override def add(category: NewCategory): ConnectionIO[ExistingCategory] =
        insertIntoReturning(
          Categories,
          NonEmptyList.of(_.name_ --> category.name),
          _.*
        )
          .queryOf(Categories.*)
          .unique
          .map(Tuples.from[ExistingCategory](_))

      override def find(
          id: CategoryId
      ): ConnectionIO[Option[ExistingCategory]] =
        sql"""
        SELECT ${Categories.*}
        FROM ${Categories}
        WHERE ${Categories.id === id}
        """
          .queryOf(Categories.*)
          .option
          .map(_.map(Tuples.from[ExistingCategory](_)))

      override def findAll: ConnectionIO[List[ExistingCategory]] =
        sql"""
        SELECT ${Categories.*}
        FROM ${Categories}
        """
          .queryOf(Categories.*)
          .to[List]
          .map(_.map(Tuples.from[ExistingCategory](_)))

private[library] object Categories extends TableDefinition("categories"):
  val id    = Column[CategoryId]("id")
  val name_ = Column[CategoryName]("name")

  val * = Columns((id, name_))
