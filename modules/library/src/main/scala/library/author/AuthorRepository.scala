package library.author

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import core.Tuples
import db.fragments.*
import doobie.*
import doobie.implicits.*
import library.author.domain.*
import neotype.interop.doobie.given

trait AuthorRepository[F[_]]:
  def add(author: NewAuthor): F[ExistingAuthor]
  def find(id: AuthorId): F[Option[ExistingAuthor]]
  def findAll: F[List[ExistingAuthor]]
  def findByIds(ids: List[AuthorId]): F[List[ExistingAuthor]]
  def findOrAdd(authors: Set[AuthorName]): F[Set[ExistingAuthor]]
  def findAssetsByAuthor(authorId: AuthorId): F[List[library.asset.domain.ExistingAsset]]

object AuthorRepository:
  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): AuthorRepository[F] =
    new:
      override def add(author: NewAuthor): F[ExistingAuthor] =
        insertIntoReturning(
          Authors,
          NonEmptyList.of(_.name_ --> author.name),
          _.*
        )
          .queryOf(Authors.*)
          .unique
          .transact(xa)
          .map: row =>
            Tuples.from[ExistingAuthor](row)

      private def add(authors: Set[NewAuthor]) =
        authors.toList
          .traverse: author =>
            insertIntoReturning(
              Authors,
              NonEmptyList.of(_.name_ --> author.name),
              _.*
            ).queryOf(Authors.*).unique
          .transact(xa)
          .map: rows =>
            rows.map(Tuples.from[ExistingAuthor](_)).toSet

      override def find(id: AuthorId): F[Option[ExistingAuthor]] =
        sql"""
        SELECT ${Authors.*}
        FROM ${Authors}
        WHERE ${Authors.id === id}
        """
          .queryOf(Authors.*)
          .option
          .transact(xa)
          .map: row =>
            row.map(Tuples.from[ExistingAuthor](_))

      override def findAll: F[List[ExistingAuthor]] =
        sql"""
        SELECT ${Authors.*}
        FROM ${Authors}
        ORDER BY ${Authors.name_}
        """
          .queryOf(Authors.*)
          .to[List]
          .transact(xa)
          .map(_.map(Tuples.from[ExistingAuthor](_)))

      override def findByIds(ids: List[AuthorId]): F[List[ExistingAuthor]] =
        if ids.isEmpty then List.empty[ExistingAuthor].pure[F]
        else
          val idsFragment = ids.map(id => fr"$id").intercalate(fr",")
          sql"""
          SELECT ${Authors.*}
          FROM ${Authors}
          WHERE ${Authors.id} IN ($idsFragment)
          """
            .queryOf(Authors.*)
            .to[List]
            .transact(xa)
            .map: rows =>
              rows.map(Tuples.from[ExistingAuthor](_))

      override def findOrAdd(authors: Set[AuthorName]): F[Set[ExistingAuthor]] =
        if authors.isEmpty then Set.empty.pure
        else
          for
            existingAuthors <- findByNames(authors)
            newAuthors <- add(authors.diff(existingAuthors.map(_.name)).map(NewAuthor.apply))
          yield existingAuthors ++ newAuthors

      override def findAssetsByAuthor(
          authorId: AuthorId
      ): F[List[library.asset.domain.ExistingAsset]] =
        sql"""
        SELECT a.id, a.title, a.category_id
        FROM assets a
        INNER JOIN assets_authors aa ON aa.asset_id = a.id
        WHERE aa.author_id = $authorId
        ORDER BY a.title
        """
          .query[(library.asset.domain.AssetId, library.asset.domain.AssetTitle, Option[library.category.domain.CategoryId])]
          .to[List]
          .transact(xa)
          .map: rows =>
            rows.map: (id, title, categoryId) =>
              library.asset.domain.ExistingAsset(id, title, categoryId, List(authorId))

      private def findByNames(names: Set[AuthorName]): F[Set[ExistingAuthor]] =
        val namesFragment =
          names.toList.map(name => fr"$name").intercalate(fr",")
        sql"""
        SELECT ${Authors.*}
        FROM ${Authors}
        WHERE ${Authors.name_} IN ($namesFragment)
        """
          .queryOf(Authors.*)
          .to[Set]
          .transact(xa)
          .map: rows =>
            rows.map(Tuples.from[ExistingAuthor](_))

private[library] object Authors extends TableDefinition("authors"):
  val id    = Column[AuthorId]("id")
  val name_ = Column[AuthorName]("name")

  val * = Columns((id, name_))
