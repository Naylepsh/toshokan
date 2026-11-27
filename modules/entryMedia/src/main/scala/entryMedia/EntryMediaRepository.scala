package entryMedia

import cats.data.NonEmptyList
import cats.syntax.all.*
import core.Tuples
import db.extensions.*
import doobie.*
import doobie.implicits.*
import entryMedia.domain.*
import neotype.interop.doobie.given

trait EntryMediaRepository:
  def findAll: ConnectionIO[List[EntryMedia]]
  def findById(id: EntryMediaId): ConnectionIO[Option[EntryMedia]]
  def findPage(
      offset: Int,
      limit: Int,
      query: SearchQuery
  ): ConnectionIO[List[EntryMedia]]
  def count(query: SearchQuery): ConnectionIO[Int]
  def findTags(
      ids: List[EntryMediaId]
  ): ConnectionIO[Map[EntryMediaId, List[Tag]]]

  /** Replaces all tags of the media; tags no media uses anymore are removed */
  def setTags(id: EntryMediaId, tags: List[Tag]): ConnectionIO[Unit]

  /** Leaves the asset / entry links untouched and clears `missingSince` */
  def upsert(
      name: EntryMediaName,
      kind: EntryMediaKind,
      path: String,
      pageCount: Int,
      modifiedAt: Long,
      size: Long,
      thumbnailExt: Option[String]
  ): ConnectionIO[EntryMediaId]
  def markMissing(ids: List[EntryMediaId], since: Long): ConnectionIO[Unit]

object EntryMediaRepository:
  val make: EntryMediaRepository = new:
    override def findAll = select(fr"").to[List]

    override def findById(id: EntryMediaId) =
      select(fr"WHERE ${EntryMediaTable.id === id}").option

    override def findPage(offset: Int, limit: Int, query: SearchQuery) =
      select(
        matching(query) ++
          fr"ORDER BY ${EntryMediaTable.mediaName} LIMIT $limit OFFSET $offset"
      ).to[List]

    override def count(query: SearchQuery) =
      (sql"SELECT COUNT(*) FROM ${EntryMediaTable} " ++ matching(query))
        .query[Int]
        .unique

    override def findTags(ids: List[EntryMediaId]) =
      NonEmptyList
        .fromList(ids)
        .fold(Map.empty[EntryMediaId, List[Tag]].pure[ConnectionIO]): ids =>
          sql"""
            SELECT emt.entry_media_id, t.namespace, t.name
            FROM entry_media_tags emt
            JOIN tags t ON t.id = emt.tag_id
            WHERE ${Fragments.in(fr"emt.entry_media_id", ids)}
            ORDER BY t.namespace, t.name
          """
            .query[(EntryMediaId, TagNamespace, TagName)]
            .to[List]
            .map(
              _.groupMap((id, _, _) => id)((_, namespace, name) =>
                Tag(namespace, name)
              )
            )

    override def setTags(id: EntryMediaId, tags: List[Tag]) =
      val distinct = tags.distinct
      for
        _ <- Update[(TagNamespace, TagName)](
          "INSERT OR IGNORE INTO tags (namespace, name) VALUES (?, ?)"
        ).updateMany(distinct.map(t => (t.namespace, t.name)))
        _ <-
          sql"DELETE FROM entry_media_tags WHERE entry_media_id = $id".update.run
        _ <- Update[(EntryMediaId, TagNamespace, TagName)](
          """INSERT INTO entry_media_tags (entry_media_id, tag_id)
             SELECT ?, id FROM tags WHERE namespace = ? AND name = ?"""
        ).updateMany(distinct.map(t => (id, t.namespace, t.name)))
        _ <-
          sql"DELETE FROM tags WHERE id NOT IN (SELECT tag_id FROM entry_media_tags)".update.run
      yield ()

    // instr instead of LIKE so that % and _ in the query are taken literally
    private def matching(query: SearchQuery) =
      val words = query.words.map: word =>
        fr"instr(lower(${EntryMediaTable.mediaName}), lower($word)) > 0"
      val tags = query.tags.map: tag =>
        fr"""EXISTS (
          SELECT 1 FROM entry_media_tags emt
          JOIN tags t ON t.id = emt.tag_id
          WHERE emt.entry_media_id = ${EntryMediaTable}.${EntryMediaTable.id}
            AND t.namespace = ${tag.namespace}
            AND t.name = ${tag.name}
        )"""
      NonEmptyList
        .fromList(words ++ tags)
        .fold(fr"")(conditions =>
          fr"WHERE" ++ Fragments.and(conditions) ++ fr" "
        )

    override def upsert(
        name: EntryMediaName,
        kind: EntryMediaKind,
        path: String,
        pageCount: Int,
        modifiedAt: Long,
        size: Long,
        thumbnailExt: Option[String]
    ) =
      sql"""
        ${EntryMediaTable.insertIntoX(
          NonEmptyList.of(
            _.mediaName --> name,
            _.kind --> kind,
            _.path --> path,
            _.pageCount --> pageCount,
            _.modifiedAt --> modifiedAt,
            _.size --> size,
            _.thumbnailExt --> thumbnailExt
          )
        )}
        ON CONFLICT (${EntryMediaTable.mediaName}) DO UPDATE SET
          ${EntryMediaTable.kind} = EXCLUDED.${EntryMediaTable.kind},
          ${EntryMediaTable.path} = EXCLUDED.${EntryMediaTable.path},
          ${EntryMediaTable.pageCount} = EXCLUDED.${EntryMediaTable.pageCount},
          ${EntryMediaTable.modifiedAt} = EXCLUDED.${EntryMediaTable.modifiedAt},
          ${EntryMediaTable.size} = EXCLUDED.${EntryMediaTable.size},
          ${EntryMediaTable.thumbnailExt} = EXCLUDED.${EntryMediaTable.thumbnailExt},
          ${EntryMediaTable.missingSince} = NULL
        RETURNING ${EntryMediaTable.id}
      """.query[EntryMediaId].unique

    override def markMissing(ids: List[EntryMediaId], since: Long) =
      ids.traverse_ : id =>
        sql"""
          UPDATE ${EntryMediaTable}
          SET ${EntryMediaTable.missingSince} = $since
          WHERE ${EntryMediaTable.id === id}
        """.update.run

    private def select(rest: Fragment) =
      (sql"SELECT ${EntryMediaTable.*} FROM ${EntryMediaTable} " ++ rest)
        .queryOf(EntryMediaTable.*)
        .map(Tuples.from[EntryMedia](_))

private given Meta[EntryMediaKind] =
  Meta[String].imap(EntryMediaKind.valueOf)(_.toString)

private object EntryMediaTable extends TableDefinition("entry_media"):
  val id           = Column[EntryMediaId]("id")
  val mediaName    = Column[EntryMediaName]("name")
  val kind         = Column[EntryMediaKind]("kind")
  val path         = Column[String]("path")
  val pageCount    = Column[Int]("page_count")
  val modifiedAt   = Column[Long]("modified_at")
  val size         = Column[Long]("size")
  val thumbnailExt = Column[Option[String]]("thumbnail_ext")
  val missingSince = Column[Option[Long]]("missing_since")

  val * = Columns(
    (
      id,
      mediaName,
      kind,
      path,
      pageCount,
      modifiedAt,
      size,
      thumbnailExt,
      missingSince
    )
  )
