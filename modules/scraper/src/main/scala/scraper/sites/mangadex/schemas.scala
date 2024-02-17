package scraper.sites.mangadex

import io.circe.Decoder

case class ChapterMetadata(
    chapter: String,
    externalUrl: Option[String],
    createdAt: String
) derives Decoder

case class Chapter(id: String, attributes: ChapterMetadata) derives Decoder

case class GetMangaFeedResponse(data: List[Chapter]) derives Decoder
