package library

import cats.effect.{ Concurrent, IO, MonadCancelThrow }
import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import io.github.arainko.ducktape.*
import library.domain.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.*
import org.http4s.server.Router
import org.typelevel.ci.CIString

class AssetController[F[_]: MonadCancelThrow: Concurrent, A](
    service: AssetService[F],
    view: AssetView[F, A]
)(using EntityEncoder[F, A]) extends Http4sDsl[F]:
  import AssetController.{ *, given }

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      service.findAll.flatMap: assetsWithEntries =>
        Ok(view.renderAssets(assetsWithEntries), `Content-Type`(view.mediaType))

    case GET -> Root / "new" =>
      Ok(view.renderForm(None), `Content-Type`(view.mediaType))

    case GET -> Root / "edit" / AssetIdVar(id) =>
      service.find(id).flatMap:
        case Some(asset, _) =>
          Ok(view.renderForm(asset.some), `Content-Type`(view.mediaType))
        case None =>
          NotFound(s"Asset ${id} not found")

    case req @ POST -> Root =>
      withJsonErrorsHandled[NewAsset](req): newAsset =>
        service.add(newAsset).flatMap:
          case Left(AddAssetError.AssetAlreadyExists) =>
            Conflict(s"${newAsset.title} already exists")
          case Right(asset) =>
            Ok(
              asset.id.value.toString,
              addRedirectHeaderIfHtmxRequest(
                req,
                s"assets/edit/${asset.id}"
              )
            )

    case req @ PUT -> Root / AssetIdVar(id) =>
      withJsonErrorsHandled[NewAsset](req): newAsset =>
        val asset = newAsset.asExisting(id)
        service.update(asset) *> Ok(
          asset.id.value.toString,
          addRedirectHeaderIfHtmxRequest(req, "/assets")
        )

    case DELETE -> Root / AssetIdVar(id) =>
      service.delete(id) *> Ok()

    case req @ POST -> Root / AssetIdVar(assetId) / "scraping" / "configs" =>
      withJsonErrorsHandled[NewAssetScrapingConfigDTO](req): newConfig =>
        service.add(newConfig.toDomain(assetId)).flatMap:
          case Left(AddScrapingConfigError.ConfigAlreadyExists) =>
            Conflict(s"${newConfig.uri} already exists")
          case Left(AddScrapingConfigError.AssetDoesNotExists) =>
            BadRequest(s"Asset ${assetId} does not exist")
          case Right(config) =>
            Ok(config.id.value.toString)

    case req @ DELETE -> Root
        / AssetIdVar(_)
        / "scraping"
        / "configs"
        / AssetScrapingConfigIdVar(id) =>
      service.deleteScrapingConfig(id) *> Ok()

    case GET -> Root / "assets" / "entries-by-release-date" =>
      service.findAllGroupedByReleaseDate.flatMap: results =>
        ???

  val routes = Router("assets" -> httpRoutes)

  private type EntityDecoderF[A] = EntityDecoder[F, A]

  private def withJsonErrorsHandled[A](request: Request[F])(using
  EntityDecoder[F, A]): (A => F[Response[F]]) => F[Response[F]] = f =>
    request.as[A].attempt.flatMap:
      case Left(InvalidMessageBodyFailure(details, cause)) =>
        BadRequest(cause.map(_.toString).getOrElse(details))
      case Left(error) =>
        println(s"[ERROR]: $error")
        InternalServerError("Something went wrong")
      case Right(a) => f(a)

object AssetController:
  object AssetIdVar:
    def unapply(str: String): Option[AssetId] =
      str.toIntOption.map(AssetId(_))

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

  given [F[_]: Concurrent]: EntityDecoder[F, NewAsset] = jsonOf[F, NewAsset]
  given [F[_]: Concurrent]: EntityDecoder[F, NewAssetScrapingConfig] =
    jsonOf[F, NewAssetScrapingConfig]
  given [F[_]: Concurrent]: EntityDecoder[F, NewAssetScrapingConfigDTO] =
    jsonOf[F, NewAssetScrapingConfigDTO]

  private def addRedirectHeaderIfHtmxRequest[F[_]](
      request: Request[F],
      redirectTo: String
  ): List[Header.Raw] =
    if request.headers.get(CIString("HX-Request")).isDefined then
      Header.Raw(CIString("HX-Location"), redirectTo) :: Nil
    else Nil
