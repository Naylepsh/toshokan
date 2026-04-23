package assetScraping.schedules

import scala.collection.immutable.SortedSet

import cats.data.{NonEmptyList, NonEmptySet}
import cats.syntax.all.*
import core.given
import doobie.*
import doobie.implicits.*
import library.category.domain.CategoryId
import neotype.interop.cats.given
import neotype.interop.doobie.given

import domain.*

trait ScheduleRepository:
  def findAllCategorySchedules: ConnectionIO[List[ScrapingSchedule.Category]]
  def findAuthorSchedule: ConnectionIO[Option[ScrapingSchedule.Authors]]
  def findByCategoryIds(
      categoryIds: NonEmptyList[CategoryId]
  ): ConnectionIO[List[ScrapingSchedule.Category]]
  def add(schedule: ScrapingSchedule.Category): ConnectionIO[Unit]
  def update(
      schedule: ScrapingSchedule.Category
  ): ConnectionIO[Either[UpdateScheduleError, Unit]]

object ScheduleRepository:
  val make: ScheduleRepository =
    new:
      override def findAllCategorySchedules
          : ConnectionIO[List[ScrapingSchedule.Category]] =
        sql"""
          SELECT ${Schedules.allWithoutId}
          FROM ${Schedules}
          """
          .queryOf(Schedules.allWithoutId)
          .to[List]
          .map: results =>
            results
              .groupBy(_._1)
              .map[ScrapingSchedule.Category]: (assetId, grouped) =>
                ScrapingSchedule.Category(
                  assetId,
                  NonEmptySet
                    .fromSetUnsafe(grouped.map(_._2).toSet.to(SortedSet))
                )
              .toList

      override def findAuthorSchedule
          : ConnectionIO[Option[ScrapingSchedule.Authors]] =
        sql"SELECT day_of_week FROM author_scraping_schedules"
          .query[DayOfTheWeek]
          .to[List]
          .map: days =>
            NonEmptySet
              .fromSet(SortedSet.from(days))
              .map(ScrapingSchedule.Authors(_))

      override def add(
          schedule: ScrapingSchedule.Category
      ): ConnectionIO[Unit] =
        addUnsafe(schedule.categoryId, schedule.days)

      override def update(
          schedule: ScrapingSchedule.Category
      ): ConnectionIO[Either[UpdateScheduleError, Unit]] =
        findByCategoryId(schedule.categoryId).flatMap:
          case None =>
            UpdateScheduleError.ScheduleDoesNotExist.asLeft.pure[ConnectionIO]
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
              .getOrElse(().pure[ConnectionIO])
            val removeDays = NonEmptyList
              .fromList(daysToRemove)
              .map: days =>
                remove(schedule.categoryId, days)
              .getOrElse(().pure[ConnectionIO])
            (addDays *> removeDays).map(_.asRight)

      override def findByCategoryIds(
          categoryIds: NonEmptyList[CategoryId]
      ): ConnectionIO[List[ScrapingSchedule.Category]] =
        val query = sql"""
          SELECT ${Schedules.allWithoutId}
          FROM ${Schedules}
          WHERE """ ++ Fragments.in(Schedules.categoryId, categoryIds)

        query
          .queryOf(Schedules.allWithoutId)
          .to[List]
          .map: results =>
            results
              .groupBy(_._1)
              .map[ScrapingSchedule.Category]: (categoryId, grouped) =>
                val (head :: tail) = grouped: @unchecked
                ScrapingSchedule.Category(
                  categoryId,
                  NonEmptySet.of(head._2, tail.map(_._2)*)
                )
              .toList

      private def findByCategoryId(
          categoryId: CategoryId
      ): ConnectionIO[Option[ScrapingSchedule]] =
        sql"""
          SELECT ${Schedules.allWithoutId}
          FROM ${Schedules}
          WHERE ${Schedules.categoryId === categoryId}
          """
          .queryOf(Schedules.allWithoutId)
          .to[List]
          .map:
            case Nil => None
            case (categoryId, day) :: tail =>
              ScrapingSchedule
                .Category(
                  categoryId,
                  NonEmptySet.of(day, tail.map(_._2)*)
                )
                .some

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
          .void

      private def remove(
          categoryId: CategoryId,
          days: NonEmptyList[DayOfTheWeek]
      ) =
        val query = sql"""
          DELETE FROM ${Schedules}
          WHERE ${Schedules.categoryId === categoryId} 
          AND """ ++ Fragments.in(Schedules.day, days)
        query.update.run.void

private object Schedules extends TableDefinition("scraping_schedules"):
  val id         = Column[Long]("id")
  val categoryId = Column[CategoryId]("category_id")
  val day        = Column[DayOfTheWeek]("day_of_week")

  val allWithoutId = Columns((categoryId, day))
