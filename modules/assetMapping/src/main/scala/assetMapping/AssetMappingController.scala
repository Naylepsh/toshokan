package assetMapping

import cats.effect.{Concurrent, MonadCancelThrow}
import cats.syntax.all.*
import library.AssetController.AssetIdVar
import library.AssetService
import library.domain.AssetId
import myAnimeList.domain.Term
import org.http4s.*
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.headers.*
import org.http4s.server.Router

import schemas.{*, given}

class AssetMappingController[F[_]: MonadCancelThrow: Concurrent](
    service: AssetMappingService[F],
    assetService: AssetService[F],
    view: AssetMappingView
) extends http.Controller[F]:
  import http.Controller.given

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root / AssetIdVar(id)
        / "search" :? TermQueryParam(term) =>
      service
        .searchForManga(term)
        .flatMap:
          case Left(error) => MonadCancelThrow[F].raiseError(error)
          case Right(mangaMatches) =>
            Ok(
              view.mangaMatchesPartial(id, mangaMatches),
              `Content-Type`(MediaType.text.html)
            )

    case GET -> Root / AssetIdVar(id) / "search" =>
      assetService
        .find(id)
        .flatMap:
          case None => NotFound(s"No manga with id=$id found")
          case Some(asset, _) =>
            Ok(
              view.renderMangaSearch(asset.title, asset.id),
              `Content-Type`(MediaType.text.html)
            )

    case GET -> Root / AssetIdVar(id) =>
      service
        .findAssetWithMalMapping(id)
        .flatMap:
          case Left(error) =>
            MonadCancelThrow[F].raiseError(error)
          case Right(None) =>
            SeeOther(
              Location(
                Uri.unsafeFromString(
                  s"/asset-mapping/${id}/search"
                )
              )
            )
          case Right(Some(asset, mapping)) =>
            Ok(
              view.renderMangaMalMapping(asset, mapping),
              `Content-Type`(MediaType.text.html)
            )

    case req @ POST -> Root =>
      withJsonErrorsHandled[NewMalMangaMappingDTO](req): newMalLinking =>
        service
          .assignExternalIdToManga(newMalLinking.malId, newMalLinking.assetId)
          .flatMap:
            case Left(AssetNotFound) => NotFound("Asset not found")
            case Left(CategoryNotFound | AssetIsNotManga) =>
              BadRequest("Asset illegible for MAL integration")
            case Left(
                  ExternalIdAlreadyInUse | MangaAlreadyHasExternalIdAssigned
                ) =>
              Conflict("Duplicate mangaId / externalId assignment")
            case Right(_) => Ok("")

    case DELETE -> Root / AssetIdVar(id) =>
      service.deleteMapping(id) *> Ok("")

  val routes = Router("asset-mapping" -> httpRoutes)

private given QueryParamDecoder[Term] =
  QueryParamDecoder[String].map(Term.apply)

private object TermQueryParam extends QueryParamDecoderMatcher[Term]("term")
