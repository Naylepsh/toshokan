package mangadex.schemas

import java.time.LocalDate

import cats.syntax.all.*
import io.circe.parser.*
import mangadex.schemas.feed.{Chapter, ChapterAttributes, GetMangaFeedResponse}

class SchemasSuite extends munit.FunSuite:
  import SchemasSuite.*

  test("Parse manga-feed response"):
    val result =
      parse(mangadexMangaFeedResponse).flatMap(_.as[GetMangaFeedResponse])

    assertEquals(result, expectedMangaFeedResponse.asRight)

object SchemasSuite:
  val mangadexMangaFeedResponse = """{
     "result":"ok",
     "response":"collection",
     "data":[
        {
           "id":"a61ca907-11b4-4b11-842f-6c8406f90cec",
           "type":"chapter",
           "attributes":{
              "volume":null,
              "chapter":"7",
              "title":"Un lugar para alguien especial",
              "translatedLanguage":"es-la",
              "externalUrl":null,
              "publishAt":"2024-02-13T02:28:41+00:00",
              "readableAt":"2024-02-13T02:28:41+00:00",
              "createdAt":"2024-02-13T02:28:39+00:00",
              "updatedAt":"2024-02-13T02:29:10+00:00",
              "pages":27,
              "version":3
           }
        }
     ],
     "limit":96,
     "offset":0,
     "total":1
  }""".stripMargin
  val expectedMangaFeedResponse = GetMangaFeedResponse(
    Chapter(
      id = "a61ca907-11b4-4b11-842f-6c8406f90cec",
      attributes = ChapterAttributes(
        title = Some("Un lugar para alguien especial"),
        chapter = "7",
        externalUrl = None,
        CreatedAt(LocalDate.parse("2024-02-13"))
      )
    ) :: Nil
  )
