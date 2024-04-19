package assetScraping

import cats.effect.{ Concurrent, MonadCancelThrow }
import cats.syntax.all.*
import io.circe.*
import library.AssetController.AssetIdVar
import library.domain.AssetId
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router

import domain.*

class AssetScrapingController[F[_]: MonadCancelThrow: Concurrent](
    service: AssetScrapingService[F],
    view: AssetScrapingView
) extends http.Controller[F]:
  import http.Controller.given
  import AssetScrapingController.{ *, given }

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      Ok(
        view.renderScrapingManagement,
        `Content-Type`(MediaType.text.html)
      )

    case POST -> Root =>
      service.scrapeAllEnabled.flatMap: summary =>
        Ok(
          view.scrapingSummaryPartial(summary),
          `Content-Type`(MediaType.text.html)
        )

    case GET -> Root / "assets" / AssetIdVar(assetId) / "configs" =>
      service.findByAssetId(assetId).flatMap:
        case Left(FindScrapingConfigError.AssetDoesNotExists) =>
          NotFound(s"Asset with id:$assetId could not be found")
        case Right(asset, configs) =>
          Ok(
            view.renderForms(asset, configs),
            `Content-Type`(MediaType.text.html)
          )

    case req @ POST -> Root / "assets" / AssetIdVar(assetId) / "configs" =>
      withJsonErrorsHandled[NewAssetScrapingConfigDTO](req): newConfig =>
        newConfig.toDomain(assetId) match
          case Left(error) =>
            BadRequest(s"Invalid config: ${error}")
          case Right(newConfig) =>
            service.add(newConfig).flatMap:
              case Left(AddScrapingConfigError.ConfigAlreadyExists) =>
                Conflict(s"${newConfig.uri} already exists")
              case Left(AddScrapingConfigError.AssetDoesNotExists) =>
                BadRequest(s"Asset ${assetId} does not exist")
              case Right(config) =>
                Ok(view.renderConfigRow(assetId, config.some))

    case req @ DELETE -> Root
        / "assets"
        / AssetIdVar(assetId)
        / "configs"
        / AssetScrapingConfigIdVar(id) =>
      service.delete(id) *> Ok()

  val routes = Router("asset-scraping" -> httpRoutes)

object AssetScrapingController:
  object AssetScrapingConfigIdVar:
    def unapply(str: String): Option[AssetScrapingConfigId] =
      str.toIntOption.map(AssetScrapingConfigId(_))

  given Decoder[IsConfigEnabled] = new Decoder[IsConfigEnabled]:
    private def makeErrorMessage(c: HCursor): String =
      s"""${c.value.toString} is not one of [true, false, "true", "false", "on"]"""

    final def apply(c: HCursor): Decoder.Result[IsConfigEnabled] =
      /**
       * HTML form sends checkbox value as either "on" or no value at all.
       * Hence this scuffed handling.
       *
       * This could be simplified to just:
       * {{{
       * c.as[String].flatMap:
       *   case "on" => Right(IsConfigEnabled(true))
       *   case other => Left(DecodingFailure(s"$other is not a valid value"))
       * }}}
       * if we were to allow only the HTML form payload,
       * but I'm keeping it more complex for the lulz
       */
      c.as[Boolean] match
        case Right(bool) => Right(IsConfigEnabled(bool))
        case Left(_) => c.as[String] match
            case Right("true" | "on") => Right(IsConfigEnabled(true))
            case Right("false")       => Right(IsConfigEnabled(false))
            case _                    => Left(DecodingFailure(makeErrorMessage(c), List.empty))

  case class NewAssetScrapingConfigDTO(
      uri: ScrapingConfigUri,
      site: Site,
      isEnabled: Option[IsConfigEnabled]
  ) derives Decoder:
    def toDomain(assetId: AssetId): Either[String, NewAssetScrapingConfig] =
      NewAssetScrapingConfig(
        uri,
        site,
        isEnabled.getOrElse(IsConfigEnabled(false)),
        assetId
      )

  given [F[_]: Concurrent]: EntityDecoder[F, NewAssetScrapingConfigDTO] =
    jsonOf[F, NewAssetScrapingConfigDTO]
