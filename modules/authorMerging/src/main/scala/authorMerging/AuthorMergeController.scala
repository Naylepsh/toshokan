package authorMerging

import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.Concurrent
import cats.syntax.all.*
import core.types.AtLeastTwoUnique
import io.circe.Decoder
import library.author.domain.*
import library.author.{AuthorRepository, AuthorView}
import neotype.interop.cats.given
import neotype.interop.circe.given
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.server.Router
import org.typelevel.ci.CIString

class AuthorMergeController[F[_]: Concurrent](
    mergeService: AuthorMergeService[F],
    repository: AuthorRepository[F],
    view: AuthorView
) extends http.Controller[F]:
  import http.Controller.given
  import AuthorMergeController.{*, given}

  private val httpRoutes = HttpRoutes.of[F]:
    case req @ POST -> Root / "merge" / "preview" =>
      withJsonErrorsHandled[AuthorMergeRequest](req): merge =>
        repository
          .findByIds(merge.authorIds.toList)
          .flatMap: authors =>
            if authors.size < 2 then
              BadRequest("Some selected authors were not found")
            else
              Ok(
                view.renderAuthorMergePreview(authors),
                `Content-Type`(MediaType.text.html)
              )

    case req @ POST -> Root / "merge" =>
      withJsonErrorsHandled[AuthorMergeConfirmation](req): merge =>
        val sourceIds =
          NonEmptyList.fromListUnsafe(merge.authorIds.toSortedSet.toList)
        mergeService
          .mergeAuthors(sourceIds, merge.targetId)
          .flatMap: _ =>
            Ok(
              "",
              Header.Raw(CIString("HX-Location"), "/authors")
            )

  val routes = Router("authors" -> httpRoutes)

object AuthorMergeController:
  case class AuthorMergeRequest(authorIds: AtLeastTwoUnique[AuthorId])
      derives Decoder
  case class AuthorMergeConfirmation(
      authorIds: NonEmptySet[AuthorId],
      targetId: AuthorId
  )
  object AuthorMergeConfirmation:
    given Decoder[AuthorMergeConfirmation] =
      Decoder.forProduct2("authorIds", "targetId")(
        AuthorMergeConfirmation.apply
      )

  given [F[_]: Concurrent]: EntityDecoder[F, AuthorMergeRequest] =
    jsonOf[F, AuthorMergeRequest]
  given [F[_]: Concurrent]: EntityDecoder[F, AuthorMergeConfirmation] =
    jsonOf[F, AuthorMergeConfirmation]
