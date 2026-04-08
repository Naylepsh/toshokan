package library.author

import cats.data.NonEmptySet
import cats.effect.{Concurrent, MonadCancelThrow}
import cats.syntax.all.*
import core.types.AtLeastTwoUnique
import io.circe.Decoder
import library.asset.AssetGroup
import library.asset.AssetService
import library.author.domain.*
import library.author.schemas.AuthorIdVar
import library.asset.domain.AssetId
import neotype.interop.circe.given
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router
import org.typelevel.ci.CIString

class AuthorController[F[_]: MonadCancelThrow: Concurrent](
    repository: AuthorRepository[F],
    assetService: AssetService[F],
    view: AuthorView
) extends http.Controller[F]:
  import http.Controller.given
  import AuthorController.{*, given}

  private val httpRoutes = HttpRoutes.of[F]:
    case GET -> Root =>
      repository.findAll.flatMap: authors =>
        Ok(
          view.renderAuthors(authors),
          `Content-Type`(MediaType.text.html)
        )

    case GET -> Root / AuthorIdVar(id) =>
      repository
        .find(id)
        .flatMap:
          case None => NotFound(s"Author ${id} not found")
          case Some(author) =>
            repository
              .findAssetsByAuthor(id)
              .flatMap: assets =>
                val grouped = AssetGroup.fromAssets(assets)
                Ok(
                  view.renderAuthor(author, grouped),
                  `Content-Type`(MediaType.text.html)
                )

    case req @ POST -> Root / AuthorIdVar(authorId) / "merge" / "preview" =>
      withJsonErrorsHandled[MergeRequest](req): merge =>
        repository
          .findAssetsByAuthor(authorId)
          .flatMap: allAssets =>
            val selected = allAssets.filter(a => merge.assetIds.contains(a.id))
            if selected.size != merge.assetIds.toList.size then
              BadRequest("Some selected assets were not found")
            else
              Ok(
                view.renderMergePreview(authorId, selected),
                `Content-Type`(MediaType.text.html)
              )

    case req @ POST -> Root / AuthorIdVar(authorId) / "merge" =>
      withJsonErrorsHandled[MergeConfirmation](req): merge =>
        merge.assetIds.toList
          .traverse_(sourceId =>
            assetService.mergeAssets(sourceId, merge.targetId)
          )
          .flatMap: _ =>
            Ok(
              "",
              Header.Raw(CIString("HX-Location"), s"/authors/$authorId")
            )

  val routes = Router("authors" -> httpRoutes)

object AuthorController:
  case class MergeRequest(assetIds: AtLeastTwoUnique[AssetId]) derives Decoder
  case class MergeConfirmation(
      assetIds: NonEmptySet[AssetId],
      targetId: AssetId
  )
  object MergeConfirmation:
    import neotype.interop.cats.given
    given Decoder[MergeConfirmation] =
      Decoder.forProduct2("assetIds", "targetId")(MergeConfirmation.apply)

  given [F[_]: Concurrent]: EntityDecoder[F, MergeRequest] =
    jsonOf[F, MergeRequest]
  given [F[_]: Concurrent]: EntityDecoder[F, MergeConfirmation] =
    jsonOf[F, MergeConfirmation]
