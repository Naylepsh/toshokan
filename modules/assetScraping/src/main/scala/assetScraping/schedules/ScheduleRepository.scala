package assetScraping.schedules

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobiex.*
import library.category.domain.CategoryId

import domain.*

trait ScheduleRepository[F[_]]:
  def findAll: F[List[ScrapingSchedule]]
  def findByCategoryIds(
      categoryIds: NonEmptyList[CategoryId]
  ): F[List[ScrapingSchedule]]
  def add(schedule: ScrapingSchedule): F[Unit]
  def update(
      schedule: ScrapingSchedule
  ): Raise[F, UpdateScheduleError] ?=> F[Unit]

object ScheduleRepository:
  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): ScheduleRepository[F] =
    new:

      override def findAll: F[List[ScrapingSchedule]] =
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
                ScrapingSchedule(
                  assetId,
                  NonEmptyList.fromListUnsafe(grouped.map(_._2))
                )
              .toList

      override def add(schedule: ScrapingSchedule): F[Unit] =
        addUnsafe(schedule.categoryId, schedule.days)

      override def update(
          schedule: ScrapingSchedule
      ): Raise[F, UpdateScheduleError] ?=> F[Unit] =
        findByCategoryId(schedule.categoryId).flatMap:
          case None => UpdateScheduleError.ScheduleDoesNotExist.raise
          case Some(existingSchedule) =>
            val daysToAdd = schedule.days.foldLeft(List.empty[DayOfTheWeek]):
              (acc, day) =>
                if existingSchedule.days.contains_(day) then acc else day :: acc
            val daysToRemove =
              existingSchedule.days.foldLeft(List.empty[DayOfTheWeek]):
                (acc, day) =>
                  if schedule.days.contains_(day) then acc else day :: acc
            val addDays = NonEmptyList
              .fromList(daysToAdd)
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
      ): F[List[ScrapingSchedule]] =
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
              .map: (categoryId, grouped) =>
                // groupyBy guarantees that `grouped` list has at least 1 element
                val (head :: tail) = grouped: @unchecked
                ScrapingSchedule(
                  categoryId,
                  NonEmptyList.of(head._2, tail.map(_._2)*)
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
              ScrapingSchedule(
                categoryId,
                NonEmptyList.of(day, tail.map(_._2)*)
              ).some

      /** This is unsafe because it makes no guarantee that category with
        * `categoryId` exists
        */
      private def addUnsafe(
          categoryId: CategoryId,
          days: NonEmptyList[DayOfTheWeek]
      ) =
        days
          .map: day =>
            insertInto(
              Schedules,
              NonEmptyList.of(_.categoryId --> categoryId, _.day --> day)
            ).update.run
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
