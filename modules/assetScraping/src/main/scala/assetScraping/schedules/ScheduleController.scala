package assetScraping.schedules

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Decoder
import library.category.CategoryService
import library.category.domain.CategoryId
import library.category.schemas.CategoryIdVar
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router

import domain.*

class ScheduleController[F[_]: MonadThrow: Concurrent](
    service: ScheduleService[F],
    categoryService: CategoryService[F],
    view: ScheduleView
) extends http.Controller[F]:
  import http.Controller.given
  import ScheduleController.*

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      service.findCategoriesOfAllSchedules.flatMap: categories =>
        Ok(
          view.renderScheduleLinks(categories),
          `Content-Type`(MediaType.text.html)
        )

    case GET -> Root / CategoryIdVar(categoryId) =>
      (service.find(categoryId), categoryService.findAll).tupled.flatMap:
        case (None, _) =>
          BadRequest(s"Schedule for categoryId=${categoryId} does not exist")
        case (schedule @ Some(_), categories) =>
          Ok(
            view.renderForm(categories, schedule),
            `Content-Type`(MediaType.text.html)
          )

    case req @ POST -> Root =>
      withJsonErrorsHandled[CreateScheduleDTO](req): schedule =>
        schedule.toDomain
          .traverse(service.add)
          .map(_.flatten)
          .flatMap:
            case Left(reason)    => BadRequest(reason.toString)
            case Right(schedule) => Ok("")

    case req @ PUT -> Root / CategoryIdVar(categoryId) =>
      withJsonErrorsHandled[UpdateScheduleDTO](req): schedule =>
        schedule
          .toDomain(categoryId)
          .traverse(service.update)
          .map(_.flatten)
          .flatMap:
            case Left(UpdateScheduleError.CategoryDoesNotExist) =>
              NotFound(s"Category with id=${categoryId} does not exist")
            case Left(UpdateScheduleError.ScheduleDoesNotExist) =>
              NotFound(s"Schedule for categoryId=${categoryId} does not exist")
            case Left(error: String) => BadRequest(error)
            case Right(schedule)     => Ok("")

    case GET -> Root / "new" =>
      categoryService.findAll.flatMap: categories =>
        Ok(
          view.renderForm(categories, None),
          `Content-Type`(MediaType.text.html)
        )

  val routes = Router("scraping-schedules" -> httpRoutes)

object ScheduleController:
  given Decoder[List[DayOfTheWeek]] =
    Decoder.decodeList[DayOfTheWeek] or Decoder[DayOfTheWeek].map(_ :: Nil)

  case class CreateScheduleDTO(
      categoryId: CategoryId,
      days: List[DayOfTheWeek]
  ) derives Decoder:
    def toDomain: Either[String, ScrapingSchedule] =
      makeScrapingSchedule(days)(categoryId)

  object CreateScheduleDTO:
    given [F[_]: Concurrent]: EntityDecoder[F, CreateScheduleDTO] =
      jsonOf[F, CreateScheduleDTO]

  case class UpdateScheduleDTO(days: List[DayOfTheWeek]) derives Decoder:
    val toDomain: CategoryId => Either[String, ScrapingSchedule] =
      makeScrapingSchedule(days)

  object UpdateScheduleDTO:
    given [F[_]: Concurrent]: EntityDecoder[F, UpdateScheduleDTO] =
      jsonOf[F, UpdateScheduleDTO]

  def makeScrapingSchedule(days: List[DayOfTheWeek])(categoryId: CategoryId) =
    NonEmptyList
      .fromList(days)
      .map(ScrapingSchedule(categoryId, _))
      .toRight("At least one day required")
