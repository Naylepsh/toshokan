package assetScraping

import cats.effect.{ Concurrent, IO, MonadCancelThrow }
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import io.github.arainko.ducktape.*
import library.AssetController.AssetIdVar
import library.AssetService
import library.domain.AssetId
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.*
import org.http4s.server.Router
import scraper.Scraper

import domain.*

// TODO: Rename to AssetScrapingConfigController?
class AssetScrapingController[F[_]: MonadCancelThrow: Concurrent, A](
    // scraper: Scraper[F],
    service: AssetScrapingService[F]
) extends http.Controller[F]:
  import AssetScrapingController.*

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root / "assets" / AssetIdVar(assetId) / "configs" =>
      // TODO: assetService should be moved to AssetScrapingService as a dependency.
      // Add a `AssetScrapingService.findByAssetId(id: AssetId): F[List[Existing...]]` method
      service.findByAssetId(assetId).flatMap:
        case Left(FindScrapingConfigError.AssetDoesNotExists) =>
          // TODO: This should be an equivalent of 404
          ???
        case Right(asset, configs) =>
          Ok(
            AssetScrapingView.renderForms(asset, configs),
            `Content-Type`(MediaType.text.html)
          )

    case req @ POST -> Root / "assets" / AssetIdVar(assetId) / "configs" =>
      withJsonErrorsHandled[NewAssetScrapingConfigDTO](req): newConfig =>
        service.add(newConfig.toDomain(assetId)).flatMap:
          case Left(AddScrapingConfigError.ConfigAlreadyExists) =>
            Conflict(s"${newConfig.uri} already exists")
          case Left(AddScrapingConfigError.AssetDoesNotExists) =>
            BadRequest(s"Asset ${assetId} does not exist")
          case Right(config) =>
            Ok(config.id.value.toString)

    case req @ DELETE -> Root / AssetScrapingConfigIdVar(id) =>
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
    def toDomain(assetId: AssetId): NewAssetScrapingConfig =
      this.into[NewAssetScrapingConfig]
        .transform(
          Field.const(_.assetId, assetId),
          Field.const(_.isEnabled, isEnabled.getOrElse(IsConfigEnabled(false)))
        )

  given [F[_]: Concurrent]: EntityDecoder[F, NewAssetScrapingConfigDTO] =
    jsonOf[F, NewAssetScrapingConfigDTO]
