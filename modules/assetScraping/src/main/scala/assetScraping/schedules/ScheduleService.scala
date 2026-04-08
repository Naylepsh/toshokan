package assetScraping.schedules

import assetScraping.schedules.domain.UpdateScheduleError
import cats.data.NonEmptyList
import cats.effect.kernel.Sync
import cats.mtl.Raise
import cats.mtl.implicits.*
import cats.syntax.all.*
import core.given
import library.asset.AssetService
import library.category.CategoryService
import library.category.domain.{CategoryId, ExistingCategory}
import library.asset.domain.AssetId
import neotype.interop.cats.given

import domain.{DayOfTheWeek, ScrapingSchedule, AddScheduleError}

/**
 * TODO: Add support for ScrapingSchedule.Authors
 */

trait ScheduleService[F[_]]:
  def findAssetsEligibleForScrape: F[List[AssetId]]
  def isAuthorScrapeDay: F[Boolean]
  def find(categoryId: CategoryId): F[Option[ScrapingSchedule.Category]]
  def findCategoriesOfAllSchedules: F[List[ExistingCategory]]
  def add(
      schedule: ScrapingSchedule.Category
  ): Raise[F, AddScheduleError] ?=> F[Unit]
  def update(
      schedule: ScrapingSchedule.Category
  ): Raise[F, UpdateScheduleError] ?=> F[Unit]

object ScheduleService:
  def make[F[_]: Sync](
      repository: ScheduleRepository[F],
      assetService: AssetService[F],
      categoryService: CategoryService[F]
  ): ScheduleService[F] = new:

    override def findAssetsEligibleForScrape: F[List[AssetId]] =
      for
        schedules <- repository.findAllCategorySchedules
        categoryToAssets <- assetService.matchCategoriesToAssets(
          schedules.map(_.categoryId)
        )
        currentDayOfTheWeek <- DayOfTheWeek.now
      yield extractAssetsEligibleForScraping(
        schedules,
        categoryToAssets,
        currentDayOfTheWeek
      )

    override def isAuthorScrapeDay: F[Boolean] =
      for
        schedule <- repository.findAuthorSchedule
        today    <- DayOfTheWeek.now
      yield schedule.exists(_.days.contains_(today))

    override def find(
        categoryId: CategoryId
    ): F[Option[ScrapingSchedule.Category]] =
      repository
        .findByCategoryIds(NonEmptyList.of(categoryId))
        .map(_.headOption)

    override def findCategoriesOfAllSchedules: F[List[ExistingCategory]] =
      for
        ids <- repository.findAllCategorySchedules.map(_.map(_.categoryId))
        categories <- categoryService.find(ids)
      yield categories

    override def add(
        schedule: ScrapingSchedule.Category
    ): Raise[F, AddScheduleError] ?=> F[Unit] =
      categoryService
        .find(schedule.categoryId)
        .flatMap:
          case Some(_) => repository.add(schedule)
          case None    => AddScheduleError.CategoryDoesNotExist.raise

    override def update(
        schedule: ScrapingSchedule.Category
    ): Raise[F, UpdateScheduleError] ?=> F[Unit] =
      categoryService
        .find(schedule.categoryId)
        .flatMap:
          case Some(_) => repository.update(schedule)
          case None    => UpdateScheduleError.CategoryDoesNotExist.raise

  private def extractAssetsEligibleForScraping(
      schedules: List[ScrapingSchedule.Category],
      categoryToAssets: Map[CategoryId, List[AssetId]],
      day: DayOfTheWeek
  ) =
    schedules
      .filter: schedule =>
        schedule.days.contains_(day)
      .flatMap: schedule =>
        categoryToAssets.getOrElse(schedule.categoryId, List.empty)
