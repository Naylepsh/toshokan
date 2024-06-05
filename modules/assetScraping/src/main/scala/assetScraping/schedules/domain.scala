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

case class ScrapingSchedule private (
    categoryId: CategoryId,
    days: NonEmptyList[DayOfTheWeek]
)
object ScrapingSchedule:
  def apply(
      categoryId: CategoryId,
      days: NonEmptyList[DayOfTheWeek]
  ): ScrapingSchedule =
    val dedupedDays =
      NonEmptyList.fromListUnsafe(Set.from(days.toIterable).toList).sorted
    new ScrapingSchedule(categoryId, dedupedDays)

enum AddScheduleError:
  case CategoryDoesNotExist
