package assetScraping.schedules

import cats.effect.kernel.Sync
import cats.syntax.all.*
import library.AssetService
import library.category.CategoryService
import library.category.domain.CategoryId
import library.domain.AssetId

import domain.{DayOfTheWeek, ScrapingSchedule, AddScheduleError}

trait ScheduleService[F[_]]:
  def findAssetsEligibleForScrape: F[List[AssetId]]
  def add(schedule: ScrapingSchedule): F[Either[AddScheduleError, Unit]]

object ScheduleService:
  def make[F[_]: Sync](
      repository: ScheduleRepository[F],
      assetService: AssetService[F],
      categoryService: CategoryService[F]
  ): ScheduleService[F] = new:

    override def add(
        schedule: ScrapingSchedule
    ): F[Either[AddScheduleError, Unit]] =
      categoryService
        .find(schedule.categoryId)
        .flatMap:
          case Some(_) => repository.add(schedule).map(_.asRight)
          case None    => AddScheduleError.CategoryDoesNotExist.asLeft.pure

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
