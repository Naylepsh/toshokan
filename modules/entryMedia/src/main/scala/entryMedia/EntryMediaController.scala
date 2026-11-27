package entryMedia

import cats.effect.IO
import cats.mtl.Handle
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import entryMedia.domain.*
import org.http4s.*
import org.http4s.dsl.impl.OptionalQueryParamDecoderMatcher
import org.http4s.headers.*
import org.http4s.implicits.*
import org.http4s.server.Router

class EntryMediaController(
    repository: EntryMediaRepository,
    scanner: EntryMediaScanner,
    view: EntryMediaView,
    config: Config,
    xa: Transactor[IO]
) extends http.Controller:
  import http.Controller.given
  import EntryMediaController.*

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
        .flatMap: (mediaList, total) =>
          val totalPages = ((total + pageSize - 1) / pageSize).max(1)
          Ok(
            view.renderMedia(mediaList, current, totalPages, query),
            `Content-Type`(MediaType.text.html)
          )

    case POST -> Root / "scan" =>
      Handle
        .allow[ScanError]:
          scanner.scan *> SeeOther(Location(uri"/entry-media"))
        .rescue:
          case ScanError.RootUnreadable(root) =>
            InternalServerError(s"Root is not readable: $root")

    case GET -> Root / EntryMediaIdVar(id) / "read" =>
      repository
        .findById(id)
        .transact(xa)
        .flatMap:
          case Some(media) if media.missingSince.isEmpty =>
            Ok(view.renderReader(media), `Content-Type`(MediaType.text.html))
          case _ => NotFound()

    case GET -> Root / EntryMediaIdVar(id) / "pages" / IntVar(index) =>
      repository
        .findById(id)
        .transact(xa)
        .flatMap:
          case Some(media) if media.missingSince.isEmpty =>
            val path = java.nio.file.Path.of(media.path)
            EntryMediaReader
              .pageNames(path, media.kind)
              .flatMap: names =>
                names.lift(index) match
                  case None => NotFound()
                  case Some(name) =>
                    EntryMediaReader
                      .readPage(path, media.kind, name)
                      .flatMap: bytes =>
                        Ok(
                          bytes,
                          `Content-Type`(
                            MediaType
                              .forExtension(EntryMediaReader.extensionOf(name))
                              .getOrElse(MediaType.application.`octet-stream`)
                          )
                        )
          case _ => NotFound()

    case req @ GET -> Root / "thumbnail" / EntryMediaIdVar(id) =>
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
                  EntryMediaScanner.thumbnailPath(
                    config.thumbnailDir,
                    id,
                    ext
                  )
                ),
                Some(req)
              )
              .getOrElseF(NotFound())

  val routes = Router("entry-media" -> httpRoutes)

object EntryMediaController:
  val pageSize = 24

  object EntryMediaIdVar:
    def unapply(str: String): Option[EntryMediaId] =
      str.toLongOption.map(EntryMediaId(_))

  object OptionalPageQueryParam
      extends OptionalQueryParamDecoderMatcher[Int]("page")

  object OptionalSearchQueryParam
      extends OptionalQueryParamDecoderMatcher[String]("q")
