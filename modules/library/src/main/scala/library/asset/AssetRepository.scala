package library.asset

import cats.data.{NonEmptyList, OptionT}
import cats.syntax.all.*
import core.types.PositiveInt
import core.{Tuples, given}
import db.extensions.*
import doobie.*
import doobie.implicits.*
import io.github.arainko.ducktape.*
import library.author.domain.AuthorId
import library.category.Categories
import library.category.domain.{CategoryId, CategoryName}
import neotype.interop.doobie.given
import org.typelevel.cats.time.*

import domain.*

trait AssetRepository:
  def findAll: ConnectionIO[
    List[(ExistingAsset, Option[CategoryName], List[ExistingAssetEntry])]
  ]
  def findById(assetId: AssetId): ConnectionIO[Option[
    (ExistingAsset, List[ExistingAssetEntry])
  ]]
  def findByEntryId(
      entryId: EntryId
  ): ConnectionIO[Option[(ExistingAsset, List[ExistingAssetEntry])]]
  def findStale(
      minDaysToBeStale: PositiveInt
  ): ConnectionIO[List[(ExistingAsset, Option[DateUploaded])]]
  def add(asset: NewAsset): ConnectionIO[Either[AddAssetError, ExistingAsset]]
  def add(
      entry: NewAssetEntry
  ): ConnectionIO[Either[AddEntryError, ExistingAssetEntry]]
  def addToAsset(asset: ExistingAsset, category: CategoryId): ConnectionIO[Unit]
  def update(asset: ExistingAsset): ConnectionIO[Unit]
  def update(entry: ExistingAssetEntry): ConnectionIO[Unit]
  def delete(assetId: AssetId): ConnectionIO[Unit]
  def mergeAsset(sourceId: AssetId, targetId: AssetId): ConnectionIO[Unit]
  def matchCategoriesToAssets(
      categoryIds: NonEmptyList[CategoryId]
  ): ConnectionIO[Map[CategoryId, List[AssetId]]]
  def findOrAdd(assets: Set[NewAsset]): ConnectionIO[Set[ExistingAsset]]
  def findAssetsByAuthor(
      authorId: AuthorId
  ): ConnectionIO[List[(AssetId, AssetTitle)]]
  def relinkAuthorAssets(
      sourceAssetId: AssetId,
      targetAuthorId: AuthorId
  ): ConnectionIO[Unit]

object AssetRepository:

  private val A  = Assets `as` "a"
  private val AE = AssetEntries `as` "ae"
  private val AA = AssetsAuthors `as` "aa"
  private val C  = Categories `as` "c"
  private val findAllColumns =
    Columns(
      A(_.id),
      A(_.title),
      A(_.categoryId),
      C(_.name_).option,
      AA(_.authorId).option,
      AE(_.id).option,
      AE(_.title).option,
      AE(_.no).option,
      AE(_.uri).option,
      AE(_.dateUploaded).option
    )

  val make: AssetRepository = new:

    override def findAll: ConnectionIO[
      List[(ExistingAsset, Option[CategoryName], List[ExistingAssetEntry])]
    ] =
      sql"""
          SELECT ${findAllColumns} 
          FROM ${A}
          LEFT JOIN ${C} ON ${C(_.id)} = ${A(_.categoryId)}
          LEFT JOIN ${AA} ON ${AA(_.assetId)} = ${A(_.id)}
          LEFT JOIN ${AE} ON ${AE(_.assetId)} = ${A(_.id)}
          ORDER BY ${A(_.id)}
      """
        .queryOf(findAllColumns)
        .to[List]
        .map: rows =>
          rows
            .groupBy(row => (row._1, row._2, row._3, row._4))
            .map: (asset, records) =>
              val (id, title, categoryId, categoryName) = asset
              val authors = records.mapFilter(_._5)
              val entries = records
                .map: record =>
                  (
                    record._6,
                    record._7,
                    record._8,
                    record._9,
                    record._10,
                    id.some
                  ).tupled
                    .map(Tuples.from[ExistingAssetEntry](_))
                .collect:
                  case Some(entry) => entry
              (
                ExistingAsset(id, title, categoryId, authors),
                categoryName,
                entries
              )
            .toList

    override def findById(assetId: AssetId): ConnectionIO[Option[
      (ExistingAsset, List[ExistingAssetEntry])
    ]] =
      (
        findAsset(assetId),
        findEntries(assetId),
        findAuthors(assetId)
      ).tupled.map: (maybeAsset, entries, authors) =>
        maybeAsset.map(asset => (asset.toDomain(authors), entries))

    override def findByEntryId(
        entryId: EntryId
    ): ConnectionIO[Option[(ExistingAsset, List[ExistingAssetEntry])]] =
      (for
        assetId <- OptionT(findAssetId(entryId))
        result  <- OptionT(findById(assetId))
      yield result).value

    override def findStale(
        minDaysToBeStale: PositiveInt
    ): ConnectionIO[List[(ExistingAsset, Option[DateUploaded])]] =
      val cutoff =
        Fragment.const0(s"""date('now', '-${minDaysToBeStale} day')""")
      val findAssets = sql"""
      SELECT ${A(_.*)}, MAX(${AE(_.dateUploaded)}) AS last_upload
      FROM ${A}
      LEFT JOIN ${AE} ON ${AE(_.assetId)} = ${A(_.id)}
      GROUP BY ${A(_.id)}
      HAVING last_upload IS NULL OR date(last_upload) < ${cutoff}
      ORDER BY last_upload ASC
      """
        .query[(AssetModel, Option[DateUploaded])]
        .to[List]
      for
        assets  <- findAssets
        authors <- findAuthors(assets.map(_._1.id))
      yield assets.map: (asset, dateUploaded) =>
        (asset.toDomain(authors.getOrElse(asset.id, List.empty)), dateUploaded)

    override def add(
        asset: NewAsset
    ): ConnectionIO[Either[AddAssetError, ExistingAsset]] =
      doesAssetExist(asset.title).flatMap:
        case true => AssetAlreadyExists.asLeft.pure[ConnectionIO]
        case false =>
          for
            assetModel <- addWithoutChecking(asset)
            authors    <- findAuthors(assetModel.id)
          yield assetModel.toDomain(authors).asRight

    override def add(
        entry: NewAssetEntry
    ): ConnectionIO[Either[AddEntryError, ExistingAssetEntry]] =
      (doesAssetExist(entry.assetId), doesEntryExist(entry.uri)).tupled.flatMap:
        (assetExists, entryExists) =>
          if entryExists then
            AddEntryError.EntryAlreadyExists.asLeft.pure[ConnectionIO]
          else if !assetExists then
            AddEntryError.AssetDoesNotExists.asLeft.pure[ConnectionIO]
          else addWithoutChecking(entry).map(_.asRight)

    override def addToAsset(
        asset: ExistingAsset,
        categoryId: CategoryId
    ): ConnectionIO[Unit] =
      sql"""
      ${Assets.updateTableX(
          NonEmptyList.of(
            _.categoryId --> categoryId.some
          )
        )}
      WHERE ${Assets.id === asset.id}
      """.update.run.void

    override def update(asset: ExistingAsset): ConnectionIO[Unit] =
      sql"""
      ${Assets.updateTableX(NonEmptyList.of(_.title --> asset.title))}
      WHERE ${Assets.id === asset.id}
      """.update.run.void

    override def update(entry: ExistingAssetEntry): ConnectionIO[Unit] =
      sql"""
      ${AssetEntries.updateTableX(
          NonEmptyList.of(
            _.no --> entry.no,
            _.dateUploaded --> entry.dateUploaded
          )
        )}
      WHERE ${AssetEntries.id === entry.id}
      """.update.run.void

    override def delete(assetId: AssetId): ConnectionIO[Unit] =
      sql"DELETE FROM ${Assets} WHERE ${Assets.id} = ${assetId}".update.run.void

    override def mergeAsset(
        sourceAssetId: AssetId,
        targetAssetId: AssetId
    ): ConnectionIO[Unit] =
      for
        _ <-
          sql"UPDATE ${AssetEntries} SET ${AssetEntries.assetId} = $targetAssetId WHERE ${AssetEntries.assetId} = $sourceAssetId".update.run
        _ <-
          sql"""INSERT OR IGNORE INTO ${AssetsAuthors} (${AssetsAuthors.assetId}, ${AssetsAuthors.authorId})
              SELECT $targetAssetId, ${AssetsAuthors.authorId} FROM ${AssetsAuthors} WHERE ${AssetsAuthors.assetId} = $sourceAssetId""".update.run
        _ <-
          sql"DELETE FROM ${Assets} WHERE ${Assets.id} = $sourceAssetId".update.run
      yield ()

    override def findAssetsByAuthor(
        authorId: AuthorId
    ): ConnectionIO[List[(AssetId, AssetTitle)]] =
      val A    = Assets `as` "a"
      val AA   = AssetsAuthors `as` "aa"
      val cols = Columns(A(_.id), A(_.title))
      sql"""
      SELECT ${cols}
      FROM ${A}
      INNER JOIN ${AA} ON ${AA(_.assetId)} = ${A(_.id)}
      WHERE ${AA(_.authorId)} = $authorId
      """
        .queryOf(cols)
        .to[List]

    override def relinkAuthorAssets(
        sourceAssetId: AssetId,
        targetAuthorId: AuthorId
    ): ConnectionIO[Unit] =
      sql"""
        INSERT OR IGNORE INTO ${AssetsAuthors} (${AssetsAuthors.assetId}, ${AssetsAuthors.authorId})
        VALUES ($sourceAssetId, $targetAuthorId)
      """.update.run.void

    override def matchCategoriesToAssets(
        categoryIds: NonEmptyList[CategoryId]
    ): ConnectionIO[Map[CategoryId, List[AssetId]]] =
      val query = sql"""
      SELECT ${Assets.id}, ${Assets.categoryId}
      FROM ${Assets}
      WHERE """ ++ Fragments.in(Assets.categoryId, categoryIds)
      query
        .queryOf(Columns(Assets.id, Assets.categoryId))
        .to[List]
        .map: rows =>
          rows
            .groupBy(_._2)
            .foldLeft(Map.empty):
              case (acc, (Some(categoryId), group)) =>
                acc + (categoryId -> group.map(_._1))
              case (acc, _) => acc

    override def findOrAdd(
        assets: Set[NewAsset]
    ): ConnectionIO[Set[ExistingAsset]] =
      NonEmptyList
        .fromList(assets.toList)
        .fold(Set.empty.pure[ConnectionIO]): newAssets =>
          val titles        = newAssets.map(_.title)
          val assetsByTitle = newAssets.map(a => a.title -> a).toList.toMap
          val query = sql"""
          SELECT ${Assets.*}
          FROM ${Assets}
          WHERE """ ++ Fragments.in(Assets.title, titles)
          query
            .queryOf(Assets.*)
            .to[List]
            .map: rows =>
              val existing = rows.map: row =>
                val model = Tuples.from[AssetModel](row)
                val authors = assetsByTitle
                  .get(model.title)
                  .map(_.authors)
                  .getOrElse(List.empty)
                model.toDomain(authors)
              val existingTitles = existing.map(_.title).toSet
              val missing =
                newAssets.filter(a => !existingTitles.contains(a.title))
              (existing, missing)
            .flatMap: (existing, missing) =>
              missing.toList
                .traverse(addWithoutChecking)
                .map: added =>
                  (existing ++ added
                    .map(a => a.toDomain(assetsByTitle(a.title).authors))).toSet

    private def findAsset(assetId: AssetId): ConnectionIO[Option[AssetModel]] =
      sql"""
        SELECT ${Assets.*}
        FROM ${Assets}
        WHERE ${Assets.id === assetId}
      """
        .queryOf(Assets.*)
        .option
        .map(_.map(Tuples.from[AssetModel](_)))

    private def findAssetId(entryId: EntryId): ConnectionIO[Option[AssetId]] =
      sql"""
        SELECT ${AssetEntries.assetId}
        FROM ${AssetEntries}
        WHERE ${AssetEntries.id === entryId}
      """
        .queryOf(AssetEntries.assetId)
        .option

    private def findEntries(
        assetId: AssetId
    ): ConnectionIO[List[ExistingAssetEntry]] =
      sql"""
        SELECT ${AssetEntries.*} 
        FROM ${AssetEntries} 
        WHERE ${AssetEntries.assetId === assetId}
      """
        .queryOf(AssetEntries.*)
        .to[List]
        .map(_.map(Tuples.from[ExistingAssetEntry](_)))

    private def addWithoutChecking(asset: NewAsset): ConnectionIO[AssetModel] =
      for
        row <- Assets
          .insertIntoReturning(
            NonEmptyList.of(
              _.title --> asset.title,
              _.categoryId --> asset.categoryId
            ),
            _.*
          )
          .queryOf(Assets.*)
          .unique
        model = Tuples.from[AssetModel](row)
        _ <- asset.authors.traverse_(authorId =>
          sql"INSERT INTO ${AssetsAuthors} (${AssetsAuthors.assetId}, ${AssetsAuthors.authorId}) VALUES (${model.id}, $authorId)".update.run
        )
      yield model

    private def addWithoutChecking(
        entry: NewAssetEntry
    ): ConnectionIO[ExistingAssetEntry] =
      AssetEntries
        .insertIntoReturning(
          NonEmptyList.of(
            _.title --> entry.title,
            _.no --> entry.no,
            _.uri --> entry.uri,
            _.dateUploaded --> entry.dateUploaded,
            _.assetId --> entry.assetId
          ),
          _.*
        )
        .queryOf(AssetEntries.*)
        .unique
        .map(Tuples.from[ExistingAssetEntry](_))

    private def doesAssetExist(title: AssetTitle): ConnectionIO[Boolean] =
      sql"""
        SELECT 1
        FROM ${Assets}
        WHERE ${Assets.title} = ${title}
      """.query[Int].option.map(_.isDefined)

    private def doesAssetExist(id: AssetId): ConnectionIO[Boolean] =
      sql"""
        SELECT 1
        FROM ${Assets}
        WHERE ${Assets.id} = ${id}
      """.query[Int].option.map(_.isDefined)

    private def doesEntryExist(entryUri: EntryUri): ConnectionIO[Boolean] =
      sql"""
        SELECT 1
        FROM ${AssetEntries}
        WHERE ${AssetEntries.uri === entryUri}
      """.query[Int].option.map(_.isDefined)

    private def findAuthors(assetId: AssetId): ConnectionIO[List[AuthorId]] =
      sql"""
        SELECT ${AssetsAuthors.authorId}
        FROM ${AssetsAuthors}
        WHERE ${AssetsAuthors.assetId} = ${assetId}
      """.query[AuthorId].to[List]

    private def findAuthors(
        assetIds: List[AssetId]
    ): ConnectionIO[Map[AssetId, List[AuthorId]]] =
      NonEmptyList
        .fromList(assetIds)
        .map: assetIds =>
          val query = sql"""
            SELECT ${AssetsAuthors.authorId}, ${AssetsAuthors.assetId}
            FROM ${AssetsAuthors}
            WHERE """ ++ Fragments.in(AssetsAuthors.assetId, assetIds)
          query
            .query[(AuthorId, AssetId)]
            .to[List]
            .map(_.groupMap(_._2)(_._1))
        .getOrElse(Map.empty.pure[ConnectionIO])

private[library] object Assets extends TableDefinition("assets"):
  val id         = Column[AssetId]("id")
  val title      = Column[AssetTitle]("title")
  val categoryId = Column[Option[CategoryId]]("category_id")

  val * = Columns((id, title, categoryId))

private case class AssetModel(
    id: AssetId,
    title: AssetTitle,
    categoryId: Option[CategoryId]
):
  def toDomain(authors: List[AuthorId]): ExistingAsset =
    this.into[ExistingAsset].transform(Field.const(_.authors, authors))

private object AssetEntries extends TableDefinition("asset_entries"):
  val id           = Column[EntryId]("id")
  val title        = Column[EntryTitle]("title")
  val no           = Column[EntryNo]("no")
  val uri          = Column[EntryUri]("uri")
  val dateUploaded = Column[DateUploaded]("date_uploaded")
  val assetId      = Column[AssetId]("asset_id")

  val * = Columns((id, title, no, uri, dateUploaded, assetId))

private[library] object AssetsAuthors extends TableDefinition("assets_authors"):
  val assetId  = Column[AssetId]("asset_id")
  val authorId = Column[AuthorId]("author_id")
