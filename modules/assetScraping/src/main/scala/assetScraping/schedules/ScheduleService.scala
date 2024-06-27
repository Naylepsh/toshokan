package assetScraping.schedules

import assetScraping.schedules.domain.UpdateScheduleError
import cats.data.NonEmptyList
import cats.effect.kernel.Sync
import cats.syntax.all.*
import library.AssetService
import library.category.CategoryService
import library.category.domain.{CategoryId, ExistingCategory}
import library.domain.AssetId

import domain.{DayOfTheWeek, ScrapingSchedule, AddScheduleError}

trait ScheduleService[F[_]]:
  def findAssetsEligibleForScrape: F[List[AssetId]]
  def find(categoryId: CategoryId): F[Option[ScrapingSchedule]]
  def findCategoriesOfAllSchedules: F[List[ExistingCategory]]
  def add(schedule: ScrapingSchedule): F[Either[AddScheduleError, Unit]]
  def update(schedule: ScrapingSchedule): F[Either[UpdateScheduleError, Unit]]

object ScheduleService:
  def make[F[_]: Sync](
      repository: ScheduleRepository[F],
      assetService: AssetService[F],
      categoryService: CategoryService[F]
  ): ScheduleService[F] = new:

    override def findAssetsEligibleForScrape: F[List[AssetId]] =
      for
        schedules <- repository.findAll
        categoryToAssets <- assetService.matchCategoriesToAssets(
          schedules.map(_.categoryId)
        )
        currentDayOfTheWeek <- DayOfTheWeek.now
      yield extractAssetsEligibleForScraping(
        schedules,
        categoryToAssets,
        currentDayOfTheWeek
      )

    override def find(categoryId: CategoryId): F[Option[ScrapingSchedule]] =
      repository
        .findByCategoryIds(NonEmptyList.of(categoryId))
        .map(_.headOption)

    override def findCategoriesOfAllSchedules: F[List[ExistingCategory]] =
      for
        ids        <- repository.findAll.map(_.map(_.categoryId))
        categories <- categoryService.find(ids)
      yield categories

    override def add(
        schedule: ScrapingSchedule
    ): F[Either[AddScheduleError, Unit]] =
      categoryService
        .find(schedule.categoryId)
        .flatMap:
          case Some(_) => repository.add(schedule).map(_.asRight)
          case None    => AddScheduleError.CategoryDoesNotExist.asLeft.pure

    override def update(
        schedule: ScrapingSchedule
    ): F[Either[UpdateScheduleError, Unit]] =
      categoryService
        .find(schedule.categoryId)
        .flatMap:
          case Some(_) => repository.update(schedule)
          case None    => UpdateScheduleError.CategoryDoesNotExist.asLeft.pure

  private def extractAssetsEligibleForScraping(
      schedules: List[ScrapingSchedule],
      categoryToAssets: Map[CategoryId, List[AssetId]],
      day: DayOfTheWeek
  ) =
    schedules
      .filter: schedule =>
        schedule.days.contains_(day)
      .flatMap: schedule =>
        categoryToAssets.getOrElse(schedule.categoryId, List.empty)
