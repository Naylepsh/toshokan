package progressTracking

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import core.Tuples
import core.given
import db.extensions.*
import doobie.*
import doobie.implicits.*
import library.domain.EntryId
import neotype.interop.doobie.given
import org.typelevel.cats.time.*

import domain.{EntryProgress, DateMarkedSeen, WasEntrySeen}

trait EntryProgressRepository[F[_]]:
  def find(entryId: EntryId): F[Option[EntryProgress]]
  def setSeen(entryId: EntryId, wasSeen: WasEntrySeen): F[EntryProgress]
  def findSeenEntries: F[List[EntryId]]

object EntryProgressRepository:
  def make[F[_]: MonadCancelThrow](
      xa: Transactor[F]
  ): EntryProgressRepository[F] = new:

    override def find(entryId: EntryId): F[Option[EntryProgress]] =
      sql"""
        SELECT ${EntryProgressTable.*}
        FROM ${EntryProgressTable} 
        WHERE ${EntryProgressTable.entryId === entryId}
      """
        .queryOf(EntryProgressTable.*)
        .option
        .transact(xa)
        .map(_.map(Tuples.from[EntryProgress](_)))

    override def setSeen(
        entryId: EntryId,
        wasSeen: WasEntrySeen
    ): F[EntryProgress] =
      val now = DateMarkedSeen(java.time.LocalDateTime.now())
      sql"""
        ${EntryProgressTable.insertIntoX(
          NonEmptyList.of(
            _.entryId --> entryId,
            _.wasSeen --> wasSeen,
            _.dateMarkedSeen --> now.some
          )
        )}
        ON CONFLICT (${EntryProgressTable.entryId}) DO UPDATE SET
          ${EntryProgressTable.wasSeen} = EXCLUDED.${EntryProgressTable.wasSeen},
          ${EntryProgressTable.dateMarkedSeen} = EXCLUDED.${EntryProgressTable.dateMarkedSeen}
      """.update.run
        .transact(xa)
        .as(EntryProgress(entryId, wasSeen, Some(now)))

    override def findSeenEntries: F[List[EntryId]] =
      sql"""
        SELECT ${EntryProgressTable.entryId}
        FROM ${EntryProgressTable}
        WHERE ${EntryProgressTable.wasSeen} = true
      """
        .query[EntryId]
        .to[List]
        .transact(xa)

private object EntryProgressTable extends TableDefinition("entry_progress"):
  val entryId        = Column[EntryId]("entry_id")
  val wasSeen        = Column[WasEntrySeen]("was_seen")
  val dateMarkedSeen = Column[Option[DateMarkedSeen]]("date_marked_seen")

  val * = Columns((entryId, wasSeen, dateMarkedSeen))
