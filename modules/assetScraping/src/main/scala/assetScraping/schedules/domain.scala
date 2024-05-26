package assetScraping.schedules.domain

import java.time.{DayOfWeek, LocalDate}

import cats.data.NonEmptyList
import cats.effect.kernel.Sync
import cats.syntax.all.*
import core.{Newtype, given}
import library.category.domain.CategoryId

type DayOfTheWeek = DayOfTheWeek.Type
object DayOfTheWeek extends Newtype[DayOfWeek]:
  val fullWeek: NonEmptyList[DayOfTheWeek] =
    NonEmptyList.fromListUnsafe(DayOfWeek.values.toList.map(DayOfTheWeek(_)))

  def now[F[_]](using F: Sync[F]): F[DayOfTheWeek] =
    F.delay(DayOfTheWeek(LocalDate.now().getDayOfWeek))

type MinDaysSinceLastScrape = MinDaysSinceLastScrape.Type
object MinDaysSinceLastScrape extends Newtype[Short]

case class ScrapingSchedule private (
    categoryId: CategoryId,
    days: NonEmptyList[DayOfTheWeek],
    minDaysSinceLastScrape: MinDaysSinceLastScrape
)
object ScrapingSchedule:
  def apply(
      categoryId: CategoryId,
      days: NonEmptyList[DayOfTheWeek],
      minDaysSinceLastScrape: MinDaysSinceLastScrape
  ): Either[String, ScrapingSchedule] =
    days match
      case NonEmptyList(_, Nil) =>
        new ScrapingSchedule(categoryId, days, minDaysSinceLastScrape).asRight
      case _ =>
        val sortedDays =
          NonEmptyList.fromListUnsafe(Set.from(days.toIterable).toList).sorted
        val minDifferenceBetweenDays = sortedDays.toList
          .zip(sortedDays.tail :+ sortedDays.head)
          .map(_.ordinal - _.ordinal)
          .min
        Either.cond(
          minDifferenceBetweenDays < minDaysSinceLastScrape || sortedDays.length == 1,
          new ScrapingSchedule(
            categoryId,
            sortedDays,
            minDaysSinceLastScrape
          ),
          s"""Difference between at least one pair of days is smaller than minDaysSinceLastScrape:
          | ($minDifferenceBetweenDays vs $minDaysSinceLastScrape)"""
        )

enum AddScheduleError:
  case CategoryDoesNotExist
