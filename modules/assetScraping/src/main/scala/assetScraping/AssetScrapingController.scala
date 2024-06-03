package assetScraping

import cats.effect.{Concurrent, MonadCancelThrow}
import cats.syntax.all.*
import library.category.CategoryService
import library.category.schemas.CategoryIdVar
import org.http4s.*
import org.http4s.dsl.impl.OptionalQueryParamDecoderMatcher
import org.http4s.headers.*
import org.http4s.server.Router

class AssetScrapingController[F[_]: MonadCancelThrow: Concurrent](
    service: AssetScrapingService[F],
    categoryService: CategoryService[F],
    view: AssetScrapingView
) extends http.Controller[F]:
  import http.Controller.given
  import AssetScrapingController.*

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      categoryService.findAll.flatMap: categories =>
        Ok(
          view.renderScrapingManagement(categories),
          `Content-Type`(MediaType.text.html)
        )

    case POST -> Root :? OptionalScrapeTypeQueryParam(scrapeType) =>
      val getNewReleases = scrapeType.getOrElse(ScrapeType.Full) match
        case ScrapeType.Full =>
          service.getNewReleases
        case ScrapeType.ScheduleOnly =>
          service.getNewReleasesAccordingToSchedule
      getNewReleases.flatMap: summary =>
        Ok(
          view.scrapingSummaryPartial(summary),
          `Content-Type`(MediaType.text.html)
        )

    case POST -> Root / "category" / CategoryIdVar(categoryId) =>
      service
        .getNewReleasesOfCategory(categoryId)
        .flatMap:
          case None => NotFound(s"Category.id=${categoryId} does not exist")
          case Some(summary) =>
            Ok(
              view.scrapingSummaryPartial(summary),
              `Content-Type`(MediaType.text.html)
            )

  val routes = Router("asset-scraping" -> httpRoutes)

object AssetScrapingController:
  enum ScrapeType:
    case Full, ScheduleOnly
  object ScrapeType:
    given QueryParamDecoder[ScrapeType] = QueryParamDecoder[String].emap:
      case "full"          => ScrapeType.Full.asRight
      case "schedule-only" => ScrapeType.ScheduleOnly.asRight
      case other =>
        ParseResult.fail(
          "Invalid scrape type",
          s"${other} is not a valid scrape type"
        )

  object OptionalScrapeTypeQueryParam
      extends OptionalQueryParamDecoderMatcher[ScrapeType]("scrape-type")
