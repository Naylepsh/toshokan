package library.author

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import core.Tuples
import db.extensions.*
import doobie.*
import doobie.implicits.*
import library.asset.domain.ExistingAsset
import library.asset.{Assets, AssetsAuthors}
import library.author.domain.*
import neotype.interop.doobie.given

trait AuthorRepository[F[_]]:
  def add(author: NewAuthor): F[ExistingAuthor]
  def find(id: AuthorId): F[Option[ExistingAuthor]]
  def findAll: F[List[ExistingAuthor]]
  def findByIds(ids: List[AuthorId]): F[List[ExistingAuthor]]
  def findOrAdd(authors: Set[AuthorName]): F[Set[ExistingAuthor]]
  def findAssetsByAuthor(
      authorId: AuthorId
  ): F[List[ExistingAsset]]
  def recordAliases(
      sourceIds: NonEmptyList[AuthorId],
      targetId: AuthorId
  ): ConnectionIO[Unit]
  def deleteAuthors(ids: NonEmptyList[AuthorId]): ConnectionIO[Unit]

object AuthorRepository:
  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): AuthorRepository[F] =
    new:
      override def add(author: NewAuthor): F[ExistingAuthor] =
        Authors
          .insertIntoReturning(
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
            Authors
              .insertIntoReturning(
                NonEmptyList.of(_.name_ --> author.name),
                _.*
              )
              .queryOf(Authors.*)
              .unique
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
        NonEmptyList
          .fromList(ids)
          .fold(List.empty[ExistingAuthor].pure): nel =>
            val query = sql"""
            SELECT ${Authors.*}
            FROM ${Authors}
            WHERE """ ++ Fragments.in(Authors.id, nel)
            query
              .queryOf(Authors.*)
              .to[List]
              .transact(xa)
              .map(_.map(Tuples.from[ExistingAuthor](_)))

      override def findOrAdd(authors: Set[AuthorName]): F[Set[ExistingAuthor]] =
        if authors.isEmpty then Set.empty.pure
        else
          for
            existingAuthors <- findByNames(authors)
            remainingNames = authors.diff(existingAuthors.map(_.name))
            aliasedAuthors <- resolveAliases(remainingNames)
            newNames = remainingNames.diff(aliasedAuthors.map(_._1))
            newAuthors <- add(newNames.map(NewAuthor.apply))
          yield existingAuthors ++ aliasedAuthors.map(_._2) ++ newAuthors

      override def findAssetsByAuthor(
          authorId: AuthorId
      ): F[List[ExistingAsset]] =
        val A    = Assets `as` "a"
        val AA   = AssetsAuthors `as` "aa"
        val cols = Columns(A(_.id), A(_.title), A(_.categoryId))
        sql"""
        SELECT ${cols}
        FROM ${A}
        INNER JOIN ${AA} ON ${AA(_.assetId)} = ${A(_.id)}
        WHERE ${AA(_.authorId)} = $authorId
        ORDER BY ${A(_.title)}
        """
          .queryOf(cols)
          .to[List]
          .transact(xa)
          .map(_.map: (id, title, categoryId) =>
            ExistingAsset(id, title, categoryId, List(authorId)))

      override def recordAliases(
          sourceIds: NonEmptyList[AuthorId],
          targetId: AuthorId
      ): ConnectionIO[Unit] =
        for
          sourceAuthors <-
            (sql"""SELECT ${Authors.name_} FROM ${Authors} WHERE """ ++ Fragments
              .in(Authors.id, sourceIds))
              .queryOf(Authors.name_)
              .to[List]
          _ <- sourceAuthors.traverse_ : name =>
            sql"""
              INSERT OR IGNORE INTO ${AuthorAliases} (${AuthorAliases.aliasName}, ${AuthorAliases.authorId})
              VALUES ($name, $targetId)
            """.update.run
        yield ()

      override def deleteAuthors(
          ids: NonEmptyList[AuthorId]
      ): ConnectionIO[Unit] =
        (sql"DELETE FROM ${Authors} WHERE " ++ Fragments
          .in(Authors.id, ids)).update.run.void

      private def resolveAliases(
          names: Set[AuthorName]
      ): F[Set[(AuthorName, ExistingAuthor)]] =
        NonEmptyList
          .fromList(names.toList)
          .fold(Set.empty[(AuthorName, ExistingAuthor)].pure): nel =>
            val AA   = AuthorAliases `as` "aa"
            val A    = Authors `as` "a"
            val cols = Columns(AA(_.aliasName), A(_.id), A(_.name_))
            val query = sql"""
            SELECT ${cols}
            FROM ${AA}
            INNER JOIN ${A} ON ${A(_.id)} = ${AA(_.authorId)}
            WHERE """ ++ Fragments.in(AA(_.aliasName), nel)
            query
              .queryOf(cols)
              .to[Set]
              .transact(xa)
              .map(_.map: (alias, id, name) =>
                (alias, ExistingAuthor(id, name)))

      private def findByNames(names: Set[AuthorName]): F[Set[ExistingAuthor]] =
        NonEmptyList
          .fromList(names.toList)
          .fold(Set.empty[ExistingAuthor].pure): nel =>
            val query = sql"""
            SELECT ${Authors.*}
            FROM ${Authors}
            WHERE """ ++ Fragments.in(Authors.name_, nel)
            query
              .queryOf(Authors.*)
              .to[Set]
              .transact(xa)
              .map(_.map(Tuples.from[ExistingAuthor](_)))

private[library] object Authors extends TableDefinition("authors"):
  val id    = Column[AuthorId]("id")
  val name_ = Column[AuthorName]("name")

  val * = Columns((id, name_))

private[library] object AuthorAliases extends TableDefinition("author_aliases"):
  val aliasName = Column[AuthorName]("alias_name")
  val authorId  = Column[AuthorId]("author_id")
