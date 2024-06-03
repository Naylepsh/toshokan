package assetScraping.schedules

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Decoder.Result
import io.circe.{Decoder, HCursor}
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
    case GET -> Root / CategoryIdVar(categoryId) => ???

    case req @ POST -> Root =>
      withJsonErrorsHandled[ScheduleDTO](req): schedule =>
        schedule.toDomain
          .traverse(service.add)
          .flatMap:
            case Left(reason)    => BadRequest(reason)
            case Right(schedule) => Ok("")

    case GET -> Root / "new" =>
      categoryService.findAll.flatMap: categories =>
        Ok(view.renderForm(categories), `Content-Type`(MediaType.text.html))

  val routes = Router("scraping-schedules" -> httpRoutes)

object ScheduleController:
  given Decoder[List[DayOfTheWeek]] =
    Decoder.decodeList[DayOfTheWeek] or Decoder[DayOfTheWeek].map(_ :: Nil)

  case class ScheduleDTO(
      categoryId: CategoryId,
      days: List[DayOfTheWeek]
  ) derives Decoder:
    def toDomain =
      NonEmptyList.fromList(days) match
        case None       => "At least one day required".asLeft
        case Some(days) => ScrapingSchedule(categoryId, days).asRight

  object ScheduleDTO:
    given [F[_]: Concurrent]: EntityDecoder[F, ScheduleDTO] =
      jsonOf[F, ScheduleDTO]
