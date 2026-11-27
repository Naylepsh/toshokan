package externalChapters

import cats.data.NonEmptyList
import cats.syntax.all.*
import core.Tuples
import db.extensions.*
import doobie.*
import doobie.implicits.*
import externalChapters.domain.*
import neotype.interop.doobie.given

trait ExternalChapterRepository:
  def findAll: ConnectionIO[List[ExternalChapter]]
  def findById(id: ChapterId): ConnectionIO[Option[ExternalChapter]]
  def findPage(
      offset: Int,
      limit: Int,
      query: Option[String]
  ): ConnectionIO[List[ExternalChapter]]
  def count(query: Option[String]): ConnectionIO[Int]

  /** Leaves the asset / entry links untouched and clears `missingSince` */
  def upsert(
      name: ChapterName,
      kind: ChapterKind,
      path: String,
      pageCount: Int,
      modifiedAt: Long,
      size: Long,
      thumbnailExt: Option[String]
  ): ConnectionIO[ChapterId]
  def markMissing(ids: List[ChapterId], since: Long): ConnectionIO[Unit]

object ExternalChapterRepository:
  val make: ExternalChapterRepository = new:
    override def findAll = select(fr"").to[List]

    override def findById(id: ChapterId) =
      select(fr"WHERE ${Chapters.id === id}").option

    override def findPage(offset: Int, limit: Int, query: Option[String]) =
      select(
        matching(query) ++
          fr"ORDER BY ${Chapters.chapterName} LIMIT $limit OFFSET $offset"
      ).to[List]

    override def count(query: Option[String]) =
      (sql"SELECT COUNT(*) FROM ${Chapters} " ++ matching(query))
        .query[Int]
        .unique

    // instr instead of LIKE so that % and _ in the query are taken literally
    private def matching(query: Option[String]) =
      query.fold(fr"")(q =>
        fr"WHERE instr(lower(${Chapters.chapterName}), lower($q)) > 0 "
      )

    override def upsert(
        name: ChapterName,
        kind: ChapterKind,
        path: String,
        pageCount: Int,
        modifiedAt: Long,
        size: Long,
        thumbnailExt: Option[String]
    ) =
      sql"""
        ${Chapters.insertIntoX(
          NonEmptyList.of(
            _.chapterName --> name,
            _.kind --> kind,
            _.path --> path,
            _.pageCount --> pageCount,
            _.modifiedAt --> modifiedAt,
            _.size --> size,
            _.thumbnailExt --> thumbnailExt
          )
        )}
        ON CONFLICT (${Chapters.chapterName}) DO UPDATE SET
          ${Chapters.kind} = EXCLUDED.${Chapters.kind},
          ${Chapters.path} = EXCLUDED.${Chapters.path},
          ${Chapters.pageCount} = EXCLUDED.${Chapters.pageCount},
          ${Chapters.modifiedAt} = EXCLUDED.${Chapters.modifiedAt},
          ${Chapters.size} = EXCLUDED.${Chapters.size},
          ${Chapters.thumbnailExt} = EXCLUDED.${Chapters.thumbnailExt},
          ${Chapters.missingSince} = NULL
        RETURNING ${Chapters.id}
      """.query[ChapterId].unique

    override def markMissing(ids: List[ChapterId], since: Long) =
      ids.traverse_ : id =>
        sql"""
          UPDATE ${Chapters}
          SET ${Chapters.missingSince} = $since
          WHERE ${Chapters.id === id}
        """.update.run

    private def select(rest: Fragment) =
      (sql"SELECT ${Chapters.*} FROM ${Chapters} " ++ rest)
        .queryOf(Chapters.*)
        .map(Tuples.from[ExternalChapter](_))

private given Meta[ChapterKind] =
  Meta[String].imap(ChapterKind.valueOf)(_.toString)

private object Chapters extends TableDefinition("external_chapters"):
  val id           = Column[ChapterId]("id")
  val chapterName  = Column[ChapterName]("name")
  val kind         = Column[ChapterKind]("kind")
  val path         = Column[String]("path")
  val pageCount    = Column[Int]("page_count")
  val modifiedAt   = Column[Long]("modified_at")
  val size         = Column[Long]("size")
  val thumbnailExt = Column[Option[String]]("thumbnail_ext")
  val missingSince = Column[Option[Long]]("missing_since")

  val * = Columns(
    (
      id,
      chapterName,
      kind,
      path,
      pageCount,
      modifiedAt,
      size,
      thumbnailExt,
      missingSince
    )
  )
