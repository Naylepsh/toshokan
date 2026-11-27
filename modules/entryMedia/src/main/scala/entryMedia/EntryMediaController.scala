package externalChapters

import cats.effect.IO
import cats.mtl.Handle
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import externalChapters.domain.*
import org.http4s.*
import org.http4s.dsl.impl.OptionalQueryParamDecoderMatcher
import org.http4s.headers.*
import org.http4s.implicits.*
import org.http4s.server.Router

class ExternalChapterController(
    repository: ExternalChapterRepository,
    scanner: ExternalChapterScanner,
    view: ExternalChapterView,
    config: Config,
    xa: Transactor[IO]
) extends http.Controller:
  import http.Controller.given
  import ExternalChapterController.*

  private val httpRoutes = HttpRoutes.of[IO]:
    case GET -> Root :? OptionalPageQueryParam(
          page
        ) +& OptionalSearchQueryParam(
          search
        ) =>
      val current = page.getOrElse(1).max(1)
      val query   = search.map(_.trim).filter(_.nonEmpty)
      (
        repository.findPage((current - 1) * pageSize, pageSize, query),
        repository.count(query)
      ).tupled
        .transact(xa)
        .flatMap: (chapters, total) =>
          val totalPages = ((total + pageSize - 1) / pageSize).max(1)
          Ok(
            view.renderChapters(chapters, current, totalPages, query),
            `Content-Type`(MediaType.text.html)
          )

    case POST -> Root / "scan" =>
      Handle
        .allow[ScanError]:
          scanner.scan *> SeeOther(Location(uri"/external-chapters"))
        .rescue:
          case ScanError.RootUnreadable(root) =>
            InternalServerError(s"Root is not readable: $root")

    case GET -> Root / ChapterIdVar(id) / "read" =>
      repository
        .findById(id)
        .transact(xa)
        .flatMap:
          case Some(chapter) if chapter.missingSince.isEmpty =>
            Ok(view.renderReader(chapter), `Content-Type`(MediaType.text.html))
          case _ => NotFound()

    case GET -> Root / ChapterIdVar(id) / "pages" / IntVar(index) =>
      repository
        .findById(id)
        .transact(xa)
        .flatMap:
          case Some(chapter) if chapter.missingSince.isEmpty =>
            val path = java.nio.file.Path.of(chapter.path)
            ChapterReader
              .pageNames(path, chapter.kind)
              .flatMap: names =>
                names.lift(index) match
                  case None => NotFound()
                  case Some(name) =>
                    ChapterReader
                      .readPage(path, chapter.kind, name)
                      .flatMap: bytes =>
                        Ok(
                          bytes,
                          `Content-Type`(
                            MediaType
                              .forExtension(ChapterReader.extensionOf(name))
                              .getOrElse(MediaType.application.`octet-stream`)
                          )
                        )
          case _ => NotFound()

    case req @ GET -> Root / "thumbnail" / ChapterIdVar(id) =>
      repository
        .findById(id)
        .transact(xa)
        .map(_.flatMap(_.thumbnailExt))
        .flatMap:
          case None => NotFound()
          case Some(ext) =>
            StaticFile
              .fromPath(
                fs2.io.file.Path.fromNioPath(
                  ExternalChapterScanner.thumbnailPath(
                    config.thumbnailDir,
                    id,
                    ext
                  )
                ),
                Some(req)
              )
              .getOrElseF(NotFound())

  val routes = Router("external-chapters" -> httpRoutes)

object ExternalChapterController:
  val pageSize = 24

  object ChapterIdVar:
    def unapply(str: String): Option[ChapterId] =
      str.toLongOption.map(ChapterId(_))

  object OptionalPageQueryParam
      extends OptionalQueryParamDecoderMatcher[Int]("page")

  object OptionalSearchQueryParam
      extends OptionalQueryParamDecoderMatcher[String]("q")
