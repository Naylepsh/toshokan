package assetScraping.schedules.domain

import java.time.{DayOfWeek, LocalDate}

import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.kernel.Sync
import library.category.domain.CategoryId
import scala.collection.immutable.SortedSet

type DayOfTheWeek = DayOfTheWeek.Type
object DayOfTheWeek extends neotype.Subtype[DayOfWeek]:
  val fullWeek: NonEmptyList[DayOfTheWeek] =
    NonEmptyList.fromListUnsafe(DayOfWeek.values.toList.map(DayOfTheWeek(_)))

  def now[F[_]](using F: Sync[F]): F[DayOfTheWeek] =
    F.delay(DayOfTheWeek(LocalDate.now().getDayOfWeek))

  def makeAll(days: List[DayOfTheWeek]): Option[NonEmptySet[DayOfTheWeek]] =
    NonEmptySet.fromSet(SortedSet.from(days))

enum AddScheduleError:
  case CategoryDoesNotExist

enum UpdateScheduleError:
  case CategoryDoesNotExist, ScheduleDoesNotExist

enum ScrapingSchedule:
  def days: NonEmptySet[DayOfTheWeek]

  case Category(categoryId: CategoryId, days: NonEmptySet[DayOfTheWeek])
  case Authors(days: NonEmptySet[DayOfTheWeek])
