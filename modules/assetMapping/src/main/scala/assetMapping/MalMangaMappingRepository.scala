package assetMapping

import cats.data.NonEmptyList
import cats.syntax.all.*
import core.Tuples
import db.extensions.*
import doobie.*
import doobie.implicits.*
import library.asset.domain.AssetId
import myAnimeList.domain.ExternalMangaId
import neotype.*
import neotype.interop.doobie.given

import domain.*

trait MalMangaMappingRepository:
  def assignMalIdToManga(
      externalId: ExternalMangaId,
      internalId: MangaId
  ): ConnectionIO[Unit]
  def findMalId(mangaId: MangaId): ConnectionIO[Option[ExternalMangaId]]
  def findMangaId(malId: ExternalMangaId): ConnectionIO[Option[MangaId]]
  def findMapping(
      mangaId: MangaId
  ): ConnectionIO[Option[ExistingMalMangaMapping]]
  def delete(mangaId: MangaId): ConnectionIO[Unit]
  def transferMappings(
      sourceAssetId: AssetId,
      targetAssetId: AssetId
  ): ConnectionIO[Unit]

object MalMangaMappingRepository:
  val make: MalMangaMappingRepository = new:
    override def assignMalIdToManga(
        externalId: ExternalMangaId,
        internalId: MangaId
    ): ConnectionIO[Unit] =
      MalMangaMappings
        .insertIntoX(
          NonEmptyList.of(_.mangaId --> internalId, _.malId --> externalId)
        )
        .update
        .run
        .void

    override def findMalId(
        mangaId: MangaId
    ): ConnectionIO[Option[ExternalMangaId]] =
      sql"""
      SELECT ${MalMangaMappings.malId}
      FROM ${MalMangaMappings}
      WHERE ${MalMangaMappings.mangaId === mangaId}"""
        .queryOf(MalMangaMappings.malId)
        .option

    override def findMangaId(
        malId: ExternalMangaId
    ): ConnectionIO[Option[MangaId]] =
      sql"""
      SELECT ${MalMangaMappings.mangaId}
      FROM ${MalMangaMappings}
      WHERE ${MalMangaMappings.malId === malId}"""
        .queryOf(MalMangaMappings.mangaId)
        .option

    override def findMapping(
        mangaId: MangaId
    ): ConnectionIO[Option[ExistingMalMangaMapping]] =
      sql"""
      SELECT ${MalMangaMappings.*}
      FROM ${MalMangaMappings}
      WHERE ${MalMangaMappings.mangaId === mangaId}
      """
        .queryOf(MalMangaMappings.*)
        .option
        .map(_.map(Tuples.from[ExistingMalMangaMapping](_)))

    override def delete(mangaId: MangaId): ConnectionIO[Unit] =
      sql"""
      DELETE FROM ${MalMangaMappings}
      WHERE ${MalMangaMappings.mangaId === mangaId}
      """.update.run.void

    override def transferMappings(
        sourceAssetId: AssetId,
        targetAssetId: AssetId
    ): ConnectionIO[Unit] =
      val source = MangaId(sourceAssetId.unwrap)
      val target = MangaId(targetAssetId.unwrap)
      sql"""
        INSERT OR IGNORE INTO ${MalMangaMappings} (${MalMangaMappings.mangaId}, ${MalMangaMappings.malId})
        SELECT $target, ${MalMangaMappings.malId}
        FROM ${MalMangaMappings}
        WHERE ${MalMangaMappings.mangaId} = $source
      """.update.run.void

private[assetMapping] object MalMangaMappings
    extends TableDefinition("mal_manga_mapping"):
  val id      = Column[MalMangaMappingId]("id")
  val mangaId = Column[MangaId]("manga_id")
  val malId   = Column[ExternalMangaId]("mal_id")

  val * = Columns(id, mangaId, malId)
