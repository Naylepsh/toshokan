package assetScraping.schedules

import assetScraping.schedules.domain.UpdateScheduleError
import cats.data.NonEmptyList
import cats.effect.IO
import cats.mtl.Raise
import cats.mtl.implicits.*
import cats.syntax.all.*
import core.given
import doobie.*
import doobie.implicits.toConnectionIOOps
import library.asset.AssetService
import library.asset.domain.AssetId
import library.category.CategoryService
import library.category.domain.{CategoryId, ExistingCategory}
import neotype.interop.cats.given

import domain.{DayOfTheWeek, ScrapingSchedule, AddScheduleError}

trait ScheduleService:
  def findAssetsEligibleForScrape: IO[List[AssetId]]
  def isAuthorScrapeDay: IO[Boolean]
  def find(categoryId: CategoryId): IO[Option[ScrapingSchedule.Category]]
  def findCategoriesOfAllSchedules: IO[List[ExistingCategory]]
  def add(
      schedule: ScrapingSchedule.Category
  ): Raise[IO, AddScheduleError] ?=> IO[Unit]
  def update(
      schedule: ScrapingSchedule.Category
  ): Raise[IO, UpdateScheduleError] ?=> IO[Unit]

object ScheduleService:
  def make(
      repository: ScheduleRepository,
      assetService: AssetService,
      categoryService: CategoryService,
      xa: Transactor[IO]
  ): ScheduleService = new:

    override def findAssetsEligibleForScrape: IO[List[AssetId]] =
      for
        schedules <- repository.findAllCategorySchedules.transact(xa)
        categoryToAssets <- assetService.matchCategoriesToAssets(
          schedules.map(_.categoryId)
        )
        currentDayOfTheWeek <- DayOfTheWeek.now
      yield extractAssetsEligibleForScraping(
        schedules,
        categoryToAssets,
        currentDayOfTheWeek
      )

    override def isAuthorScrapeDay: IO[Boolean] =
      for
        schedule <- repository.findAuthorSchedule.transact(xa)
        today    <- DayOfTheWeek.now
      yield schedule.exists(_.days.contains_(today))

    override def find(
        categoryId: CategoryId
    ): IO[Option[ScrapingSchedule.Category]] =
      repository
        .findByCategoryIds(NonEmptyList.of(categoryId))
        .transact(xa)
        .map(_.headOption)

    override def findCategoriesOfAllSchedules: IO[List[ExistingCategory]] =
      for
        ids <- repository.findAllCategorySchedules
          .transact(xa)
          .map(_.map(_.categoryId))
        categories <- categoryService.find(ids)
      yield categories

    override def add(
        schedule: ScrapingSchedule.Category
    ): Raise[IO, AddScheduleError] ?=> IO[Unit] =
      categoryService
        .find(schedule.categoryId)
        .flatMap:
          case Some(_) => repository.add(schedule).transact(xa)
          case None    => AddScheduleError.CategoryDoesNotExist.raise

    override def update(
        schedule: ScrapingSchedule.Category
    ): Raise[IO, UpdateScheduleError] ?=> IO[Unit] =
      categoryService
        .find(schedule.categoryId)
        .flatMap:
          case Some(_) =>
            repository
              .update(schedule)
              .transact(xa)
              .flatMap:
                case Left(error) => error.raise
                case Right(())   => ().pure
          case None => UpdateScheduleError.CategoryDoesNotExist.raise

  private def extractAssetsEligibleForScraping(
      schedules: List[ScrapingSchedule.Category],
      categoryToAssets: Map[CategoryId, List[AssetId]],
      day: DayOfTheWeek
  ) =
    schedules
      .filter(_.days.contains_(day))
      .flatMap(schedule =>
        categoryToAssets.getOrElse(schedule.categoryId, List.empty)
      )
