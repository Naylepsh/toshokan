package assetScraping.schedules

import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.MonadCancelThrow
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import core.given
import doobie.*
import doobie.implicits.*
import library.category.domain.CategoryId
import neotype.interop.cats.given
import neotype.interop.doobie.given

import domain.*
import scala.collection.immutable.SortedSet

trait ScheduleRepository[F[_]]:
  def findAllCategorySchedules: F[List[ScrapingSchedule.Category]]
  def findAuthorSchedule: F[Option[ScrapingSchedule.Authors]]
  def findByCategoryIds(
      categoryIds: NonEmptyList[CategoryId]
  ): F[List[ScrapingSchedule.Category]]
  def add(schedule: ScrapingSchedule.Category): F[Unit]
  def update(
      schedule: ScrapingSchedule.Category
  ): Raise[F, UpdateScheduleError] ?=> F[Unit]

object ScheduleRepository:
  def make[F[_]: MonadCancelThrow](
      xa: Transactor[F]
  ): ScheduleRepository[F] =
    new:
      override def findAllCategorySchedules
          : F[List[ScrapingSchedule.Category]] =
        sql"""
          SELECT ${Schedules.allWithoutId}
          FROM ${Schedules}
          """
          .queryOf(Schedules.allWithoutId)
          .to[List]
          .transact(xa)
          .map: results =>
            results
              .groupBy(_._1)
              .map: (assetId, grouped) =>
                // groupyBy guarantees that `grouped` list has at least 1 element
                val r: ScrapingSchedule.Category = ScrapingSchedule.Category(
                  assetId,
                  NonEmptySet
                    .fromSetUnsafe(grouped.map(_._2).toSet.to(SortedSet))
                )
                r
              .toList

      override def findAuthorSchedule: F[Option[ScrapingSchedule.Authors]] =
        sql"SELECT day_of_week FROM author_scraping_schedules"
          .query[DayOfTheWeek]
          .to[List]
          .transact(xa)
          .map: days =>
            NonEmptySet
              .fromSet(SortedSet.from(days))
              .map(ScrapingSchedule.Authors(_))

      override def add(schedule: ScrapingSchedule.Category): F[Unit] =
        addUnsafe(schedule.categoryId, schedule.days)

      override def update(
          schedule: ScrapingSchedule.Category
      ): Raise[F, UpdateScheduleError] ?=> F[Unit] =
        findByCategoryId(schedule.categoryId).flatMap:
          case None => UpdateScheduleError.ScheduleDoesNotExist.raise
          case Some(existingSchedule) =>
            val daysToAdd =
              schedule.days.foldLeft(SortedSet.empty[DayOfTheWeek]):
                (acc, day) =>
                  if existingSchedule.days.contains_(day) then acc
                  else acc + day
            val daysToRemove =
              existingSchedule.days.foldLeft(List.empty[DayOfTheWeek]):
                (acc, day) =>
                  if schedule.days.contains_(day) then acc else day :: acc
            val addDays = NonEmptySet
              .fromSet(daysToAdd)
              .map: days =>
                addUnsafe(schedule.categoryId, days)
              .getOrElse(MonadCancelThrow[F].unit)
            val removeDays = NonEmptyList
              .fromList(daysToRemove)
              .map: days =>
                remove(schedule.categoryId, days)
              .getOrElse(MonadCancelThrow[F].unit)
            addDays *> removeDays

      override def findByCategoryIds(
          categoryIds: NonEmptyList[CategoryId]
      ): F[List[ScrapingSchedule.Category]] =
        val query = sql"""
          SELECT ${Schedules.allWithoutId}
          FROM ${Schedules}
          WHERE """ ++ Fragments.in(Schedules.categoryId, categoryIds)

        query
          .queryOf(Schedules.allWithoutId)
          .to[List]
          .transact(xa)
          .map: results =>
            results
              .groupBy(_._1)
              .map[ScrapingSchedule.Category]: (categoryId, grouped) =>
                // groupyBy guarantees that `grouped` list has at least 1 element
                val (head :: tail) = grouped: @unchecked
                ScrapingSchedule.Category(
                  categoryId,
                  NonEmptySet.of(head._2, tail.map(_._2)*)
                )
              .toList

      private def findByCategoryId(
          categoryId: CategoryId
      ): F[Option[ScrapingSchedule]] =
        sql"""
          SELECT ${Schedules.allWithoutId}
          FROM ${Schedules}
          WHERE ${Schedules.categoryId === categoryId}
          """
          .queryOf(Schedules.allWithoutId)
          .to[List]
          .transact(xa)
          .map:
            case Nil => None
            case (categoryId, day) :: tail =>
              ScrapingSchedule
                .Category(
                  categoryId,
                  NonEmptySet.of(day, tail.map(_._2)*)
                )
                .some

      /** This is unsafe because it makes no guarantee that category with
        * `categoryId` exists
        */
      private def addUnsafe(
          categoryId: CategoryId,
          days: NonEmptySet[DayOfTheWeek]
      ) =
        days.toList
          .map: day =>
            Schedules
              .insertInto(
                NonEmptyList
                  .of(
                    Schedules.categoryId --> categoryId,
                    Schedules.day --> day
                  )
              )
              .update
              .run
          .sequence
          .transact(xa)
          .void

      private def remove(
          categoryId: CategoryId,
          days: NonEmptyList[DayOfTheWeek]
      ) =
        val query = sql"""
          DELETE FROM ${Schedules}
          WHERE ${Schedules.categoryId === categoryId} 
          AND """ ++ Fragments.in(Schedules.day, days)
        query.update.run.transact(xa).void

private object Schedules extends TableDefinition("scraping_schedules"):
  val id         = Column[Long]("id")
  val categoryId = Column[CategoryId]("category_id")
  val day        = Column[DayOfTheWeek]("day_of_week")

  val allWithoutId = Columns((categoryId, day))
