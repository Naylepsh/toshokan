package mangadex
package schemas

import java.net.URI
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}

import cats.syntax.all.*
import io.circe.Decoder

type CreatedAt = CreatedAt.Type
object CreatedAt extends neotype.Subtype[LocalDate]:
  given Decoder[CreatedAt] = Decoder[String].emap: str =>
    Either
      .catchNonFatal:
        CreatedAt:
          ZonedDateTime
            .parse(str, DateTimeFormatter.ISO_DATE_TIME)
            .toLocalDate
      .leftMap(_.toString)

object feed:
  case class ChapterAttributes(
      title: Option[String],
      chapter: Option[String],
      externalUrl: Option[String],
      createdAt: CreatedAt
  ) derives Decoder

  case class Chapter(id: String, attributes: ChapterAttributes) derives Decoder:
    val url: URI = URI(
      attributes.externalUrl
        .getOrElse(s"https://mangadex.org/chapter/${id}")
    )

  case class GetMangaFeedResponse(data: List[Chapter]) derives Decoder

object manga:
  case class MangaLinks(mal: Option[String]) derives Decoder

  case class MangaAttributes(title: Map[String, String], links: MangaLinks)
      derives Decoder:
    val preferredTitle: Option[String] =
      title
        .get("en")
        .orElse(title.get("ja"))
        .orElse(title.get("ja-ro"))

  case class RelationshipAttributes(name: Option[String]) derives Decoder

  case class Relationship(
      `type`: String,
      attributes: Option[RelationshipAttributes]
  ) derives Decoder

  case class Manga(
      attributes: MangaAttributes,
      relationships: Option[List[Relationship]]
  ) derives Decoder:
    val authorNames: List[String] =
      relationships.getOrElse(Nil).collect:
        case Relationship(t, Some(RelationshipAttributes(Some(name))))
            if t == "author" || t == "artist" =>
          name
      .distinct

  case class GetMangaResponse(data: Manga) derives Decoder

object server:
  case class Chapter(hash: String, data: List[String]) derives Decoder:
    lazy val urls: List[URI] = data.map: resource =>
      URI(s"https://uploads.mangadex.org/data/${hash}/${resource}")

  case class GetChapterFilesResponse(chapter: Chapter) derives Decoder
