package assetScraping.configs

import cats.effect.IO
import cats.mtl.Handle
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import io.circe.*
import library.author.AuthorRepository
import library.author.domain.AuthorId
import library.author.schemas.AuthorIdVar
import neotype.interop.circe.given
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router

import domain.*

class AuthorScrapingConfigController(
    service: AuthorScrapingConfigService,
    authorRepository: AuthorRepository,
    view: AuthorScrapingConfigView,
    xa: Transactor[IO]
) extends http.Controller:
  import http.Controller.given
  import AuthorScrapingController.{*, given}

  private val httpRoutes = HttpRoutes.of[IO]:

    case GET -> Root / "authors" / AuthorIdVar(authorId) / "configs" =>
      authorRepository
        .find(authorId)
        .transact(xa)
        .flatMap:
          case None => NotFound(s"Author with id:$authorId could not be found")
          case Some(author) =>
            service
              .findByAuthorId(authorId)
              .flatMap: configs =>
                Ok(
                  view.renderForms(author, configs),
                  `Content-Type`(MediaType.text.html)
                )

    case req @ POST -> Root / "authors" / AuthorIdVar(authorId) / "configs" =>
      withJsonErrorsHandled[AuthorScrapingConfigDTO](req): newConfig =>
        newConfig.toDomain(authorId) match
          case Left(error) =>
            BadRequest(s"Invalid config: ${error}")
          case Right(newConfig) =>
            Handle
              .allow[AddAuthorScrapingConfig]:
                service
                  .add(newConfig)
                  .flatMap: config =>
                    Ok(view.renderConfigRow(authorId, config.some))
              .rescue:
                case AddAuthorScrapingConfig.ConfigAlreadyExists =>
                  Conflict(s"${newConfig.uri} already exists")
                case AddAuthorScrapingConfig.AuthorDoesNotExist =>
                  BadRequest(s"Author ${authorId} does not exist")

    case req @ DELETE -> Root
        / "authors"
        / AuthorIdVar(authorId)
        / "configs"
        / AuthorScrapingConfigIdVar(id) =>
      service.delete(id) *> Ok()

  val routes = Router("author-scraping" -> httpRoutes)

object AuthorScrapingController:
  object AuthorScrapingConfigIdVar:
    def unapply(str: String): Option[AuthorScrapingConfigId] =
      str.toIntOption.map(AuthorScrapingConfigId(_))

  case class AuthorScrapingConfigDTO(
      uri: ScrapingConfigUri,
      site: AuthorSite,
      isEnabled: Option[IsConfigEnabled]
  ) derives Decoder:
    def toDomain(authorId: AuthorId): Either[String, NewAuthorScrapingConfig] =
      NewAuthorScrapingConfig.make(
        uri,
        site,
        isEnabled.getOrElse(IsConfigEnabled(false)),
        authorId
      )

  given EntityDecoder[IO, AuthorScrapingConfigDTO] =
    jsonOf[IO, AuthorScrapingConfigDTO]
