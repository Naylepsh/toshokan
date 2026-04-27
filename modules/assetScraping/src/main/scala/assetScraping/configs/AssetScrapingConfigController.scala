package assetScraping.configs

import cats.effect.IO
import cats.mtl.Handle
import cats.syntax.all.*
import io.circe.*
import library.asset.AssetController.AssetIdVar
import library.asset.domain.AssetId
import neotype.interop.circe.given
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router

import domain.*

class AssetScrapingConfigController(
    service: AssetScrapingConfigService,
    view: AssetScrapingConfigView
) extends http.Controller:
  import http.Controller.given
  import AssetScrapingController.{*, given}

  private val httpRoutes = HttpRoutes.of[IO]:

    case GET -> Root / "assets" / AssetIdVar(assetId) / "configs" =>
      Handle
        .allow[FindScrapingConfigError]:
          service
            .findByAssetId(assetId)
            .flatMap: (asset, configs) =>
              Ok(
                view.renderForms(asset, configs),
                `Content-Type`(MediaType.text.html)
              )
        .rescue:
          case FindScrapingConfigError.AssetDoesNotExists =>
            NotFound(s"Asset with id:$assetId could not be found")

    case req @ POST -> Root / "assets" / AssetIdVar(assetId) / "configs" =>
      withJsonErrorsHandled[AssetScrapingConfigDTO](req): newConfig =>
        newConfig.toDomain(assetId) match
          case Left(error) =>
            BadRequest(s"Invalid config: ${error}")
          case Right(newConfig) =>
            Handle
              .allow[AddScrapingConfigError]:
                service
                  .add(newConfig)
                  .flatMap: config =>
                    Ok(view.renderConfigRow(assetId, config.some))
              .rescue:
                case AddScrapingConfigError.ConfigAlreadyExists =>
                  Conflict(s"${newConfig.uri} already exists")
                case AddScrapingConfigError.AssetDoesNotExists =>
                  BadRequest(s"Asset ${assetId} does not exist")

    case req @ PUT -> Root / "assets" / AssetIdVar(
          assetId
        ) / "configs" / AssetScrapingConfigIdVar(configId) =>
      withJsonErrorsHandled[AssetScrapingConfigDTO](req): newConfig =>
        newConfig.toDomain(assetId, configId) match
          case Left(error) =>
            BadRequest(s"Invalid config: ${error}")
          case Right(newConfig) =>
            Handle
              .allow[UpdateScrapingConfigError]:
                service
                  .update(newConfig)
                  .flatMap: config =>
                    Ok(view.renderConfigRow(assetId, config.some))
              .rescue:
                case UpdateScrapingConfigError.ConfigDoesNotExist =>
                  BadRequest(s"Config ${configId} does not exist")
                case UpdateScrapingConfigError.AssetDoesNotExists =>
                  BadRequest(s"Asset ${assetId} does not exist")
                case UpdateScrapingConfigError.ConflictingConfigError =>
                  Conflict(s"Similar config already exists")

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

  given EntityDecoder[IO, AssetScrapingConfigDTO] =
    jsonOf[IO, AssetScrapingConfigDTO]
