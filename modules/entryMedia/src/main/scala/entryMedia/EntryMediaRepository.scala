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
      query: Option[String]
  ): ConnectionIO[List[EntryMedia]]
  def count(query: Option[String]): ConnectionIO[Int]

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

    override def findPage(offset: Int, limit: Int, query: Option[String]) =
      select(
        matching(query) ++
          fr"ORDER BY ${EntryMediaTable.mediaName} LIMIT $limit OFFSET $offset"
      ).to[List]

    override def count(query: Option[String]) =
      (sql"SELECT COUNT(*) FROM ${EntryMediaTable} " ++ matching(query))
        .query[Int]
        .unique

    // instr instead of LIKE so that % and _ in the query are taken literally
    private def matching(query: Option[String]) =
      query.fold(fr"")(q =>
        fr"WHERE instr(lower(${EntryMediaTable.mediaName}), lower($q)) > 0 "
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
