package assetScraping.configs

import cats.effect.{Concurrent, MonadCancelThrow}
import cats.syntax.all.*
import io.circe.*
import library.AssetController.AssetIdVar
import library.domain.AssetId
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router

import domain.*

class AssetScrapingConfigController[F[_]: MonadCancelThrow: Concurrent](
    service: AssetScrapingConfigService[F],
    view: AssetScrapingConfigView
) extends http.Controller[F]:
  import http.Controller.given
  import AssetScrapingController.{*, given}

  private val httpRoutes = HttpRoutes.of[F]:

    case GET -> Root / "assets" / AssetIdVar(assetId) / "configs" =>
      service
        .findByAssetId(assetId)
        .flatMap:
          case Left(FindScrapingConfigError.AssetDoesNotExists) =>
            NotFound(s"Asset with id:$assetId could not be found")
          case Right(asset, configs) =>
            Ok(
              view.renderForms(asset, configs),
              `Content-Type`(MediaType.text.html)
            )

    case req @ POST -> Root / "assets" / AssetIdVar(assetId) / "configs" =>
      withJsonErrorsHandled[AssetScrapingConfigDTO](req): newConfig =>
        newConfig.toDomain(assetId) match
          case Left(error) =>
            BadRequest(s"Invalid config: ${error}")
          case Right(newConfig) =>
            service
              .add(newConfig)
              .flatMap:
                case Left(AddScrapingConfigError.ConfigAlreadyExists) =>
                  Conflict(s"${newConfig.uri} already exists")
                case Left(AddScrapingConfigError.AssetDoesNotExists) =>
                  BadRequest(s"Asset ${assetId} does not exist")
                case Right(config) =>
                  Ok(view.renderConfigRow(assetId, config.some))

    case req @ PUT -> Root / "assets" / AssetIdVar(
          assetId
        ) / "configs" / AssetScrapingConfigIdVar(configId) =>
      withJsonErrorsHandled[AssetScrapingConfigDTO](req): newConfig =>
        newConfig.toDomain(assetId, configId) match
          case Left(error) =>
            BadRequest(s"Invalid config: ${error}")
          case Right(newConfig) =>
            service
              .update(newConfig)
              .flatMap:
                case Left(UpdateScrapingConfigError.ConfigDoesNotExist) =>
                  BadRequest(s"Config ${configId} does not exist")
                case Left(UpdateScrapingConfigError.AssetDoesNotExists) =>
                  BadRequest(s"Asset ${assetId} does not exist")
                case Left(UpdateScrapingConfigError.ConflictingConfigError) =>
                  Conflict(s"Similar config already exists")
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

  /** HTML form sends checkbox value as either "on" or no value at all. I also
    * wanted to handle booleans for pure-json payloads (for no reason other than
    * a whim). Thus this scuffed custom Decoder
    */
  given Decoder[IsConfigEnabled] =
    Decoder[Boolean].map(IsConfigEnabled.apply) or Decoder[String].emap:
      case "true" | "on" => IsConfigEnabled(true).asRight
      case "false"       => IsConfigEnabled(false).asRight
      case other =>
        s"""${other} is not one of [true, false, "true", "false", "on"]""".asLeft

  case class AssetScrapingConfigDTO(
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

    def toDomain(
        assetId: AssetId,
        configId: AssetScrapingConfigId
    ): Either[String, ExistingAssetScrapingConfig] =
      ExistingAssetScrapingConfig(
        configId,
        uri,
        site,
        isEnabled.getOrElse(IsConfigEnabled(false)),
        assetId
      )

  given [F[_]: Concurrent]: EntityDecoder[F, AssetScrapingConfigDTO] =
    jsonOf[F, AssetScrapingConfigDTO]
