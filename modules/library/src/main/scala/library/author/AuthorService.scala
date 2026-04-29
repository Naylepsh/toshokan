package library.author

import cats.effect.IO
import doobie.*
import doobie.implicits.*
import library.asset.domain.ExistingAsset
import library.author.domain.*

trait AuthorService:
  def find(id: AuthorId): IO[Option[ExistingAuthor]]
  def findAll: IO[List[ExistingAuthor]]
  def findByIds(ids: List[AuthorId]): IO[List[ExistingAuthor]]
  def findAssetsByAuthor(authorId: AuthorId): IO[List[ExistingAsset]]

object AuthorService:
  def make(repository: AuthorRepository, xa: Transactor[IO]): AuthorService =
    new:
      override def find(id: AuthorId) = repository.find(id).transact(xa)
      override def findAll            = repository.findAll.transact(xa)
      override def findByIds(ids: List[AuthorId]) =
        repository.findByIds(ids).transact(xa)
      override def findAssetsByAuthor(authorId: AuthorId) =
        repository.findAssetsByAuthor(authorId).transact(xa)
