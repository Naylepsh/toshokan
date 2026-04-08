package library.asset

import io.github.arainko.ducktape.*
import cats.data.{NonEmptyList, OptionT}
import cats.effect.MonadCancelThrow
import cats.implicits.*
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import core.{Tuples, given}
import core.types.PositiveInt
import db.extensions.*
import doobie.*
import doobie.implicits.*
import neotype.interop.doobie.given
import org.typelevel.cats.time.*

import domain.*
import library.category.domain.{CategoryId, CategoryName}
import library.category.Categories
import library.author.domain.AuthorId

/** TODO:
  *   - There are some unnecessary separate queries. For example of one refer to
  *     the snippet `(1)`. This could / should be just a single query. Arguably
  *     we could make a pseudo-hybrid approach with transactions, so instead of
  *     returnig F[???] we return `Free[ConnectionOp, Option[Int]]`
  *   - Maybe within a module (in this case library) all the tables should be
  *     available to everyone to read, but only a given repo can write? This
  *     would unlock the single-query thing
  */
// scalafmt: off
/* Snippet (1)
for
  assetModel <- addWithoutChecking(asset)
  authors    <- findAuthors(assetModel.id)
yield assetModel.toDomain(authors)
 */
// scalafmt: on

trait AssetRepository[F[_]]:
  def findAll
      : F[List[(ExistingAsset, Option[CategoryName], List[ExistingAssetEntry])]]
  def findById(assetId: AssetId): F[Option[
    (
        ExistingAsset,
        List[ExistingAssetEntry]
    )
  ]]
  def findByEntryId(
      entryId: EntryId
  ): F[Option[(ExistingAsset, List[ExistingAssetEntry])]]
  def findStale(
      minDaysToBeStale: PositiveInt
  ): F[List[(ExistingAsset, DateUploaded)]]
  def add(asset: NewAsset): Raise[F, AddAssetError] ?=> F[ExistingAsset]
  def add(
      entry: NewAssetEntry
  ): Raise[F, AddEntryError] ?=> F[ExistingAssetEntry]
  def addToAsset(asset: ExistingAsset, category: CategoryId): F[Unit]
  def update(asset: ExistingAsset): F[Unit]
  def update(entry: ExistingAssetEntry): F[Unit]
  def delete(assetId: AssetId): F[Unit]
  def mergeAssets(sourceId: AssetId, targetId: AssetId): F[Unit]
  def matchCategoriesToAssets(
      categoryIds: NonEmptyList[CategoryId]
  ): F[Map[CategoryId, List[AssetId]]]
  def findOrAdd(assets: Set[NewAsset]): F[Set[ExistingAsset]]

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

  def make[F[_]: MonadCancelThrow](xa: Transactor[F]): AssetRepository[F] = new:

    override def findAll: F[
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
        .transact(xa)
        .map: rows =>
          rows
            .groupBy(row => (row._1, row._2, row._3, row._4))
            .map: (asset, records) =>
              val (id, title, categoryId, categoryName) = asset
              val authors                               = records.mapFilter(_._5)
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

    override def findById(assetId: AssetId): F[Option[
      (
          ExistingAsset,
          List[ExistingAssetEntry]
      )
    ]] =
      /** Ideally this would be done in one query, but I cba to do the sql ->
        * domain nested transformation
        */
      (
        findAsset(assetId),
        findEntries(assetId),
        findAuthors(assetId)
      ).tupled.map: (maybeAsset, entries, authors) =>
        maybeAsset.map(asset => (asset.toDomain(authors), entries))

    override def findByEntryId(
        entryId: EntryId
    ): F[Option[(ExistingAsset, List[ExistingAssetEntry])]] =
      (for
        assetId <- OptionT(findAssetId(entryId))
        result  <- OptionT(findById(assetId))
      yield result).value

    override def findStale(
        minDaysToBeStale: PositiveInt
    ): F[List[(ExistingAsset, DateUploaded)]] =
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
        .query[(AssetModel, DateUploaded)]
        .to[List]
        .transact(xa)
      for
        assets  <- findAssets
        authors <- findAuthors(assets.map(_._1.id))
      yield assets.map: (asset, dateUploaded) =>
        (asset.toDomain(authors.getOrElse(asset.id, List.empty)), dateUploaded)

    override def add(
        asset: NewAsset
    ): Raise[F, AddAssetError] ?=> F[ExistingAsset] =
      doesAssetExist(asset.title).flatMap:
        case true => AssetAlreadyExists.raise
        case false =>
          for
            assetModel <- addWithoutChecking(asset)
            authors    <- findAuthors(assetModel.id)
          yield assetModel.toDomain(authors)

    override def add(
        entry: NewAssetEntry
    ): Raise[F, AddEntryError] ?=> F[ExistingAssetEntry] =
      (doesAssetExist(entry.assetId), doesEntryExist(entry.uri)).tupled.flatMap:
        (assetExists, entryExists) =>
          if entryExists then AddEntryError.EntryAlreadyExists.raise
          else if !assetExists then AddEntryError.AssetDoesNotExists.raise
          else addWithoutChecking(entry)

    override def addToAsset(
        asset: ExistingAsset,
        categoryId: CategoryId
    ): F[Unit] =
      sql"""
      ${Assets.updateTableX(
          NonEmptyList.of(
            _.categoryId --> categoryId.some
          )
        )}
      WHERE ${Assets.id === asset.id}
      """.update.run.transact(xa).void

    override def update(asset: ExistingAsset): F[Unit] =
      sql"""
      ${Assets.updateTableX(NonEmptyList.of(_.title --> asset.title))}
      WHERE ${Assets.id === asset.id}
      """.update.run.transact(xa).void

    override def update(entry: ExistingAssetEntry): F[Unit] =
      sql"""
      ${AssetEntries.updateTableX(
          NonEmptyList.of(
            _.no --> entry.no,
            _.dateUploaded --> entry.dateUploaded
          )
        )}
      WHERE ${AssetEntries.id === entry.id}
      """.update.run.transact(xa).void

    override def delete(assetId: AssetId): F[Unit] =
      sql"DELETE FROM ${Assets} WHERE ${Assets.id} = ${assetId}".update.run
        .transact(xa)
        .void

    override def mergeAssets(sourceId: AssetId, targetId: AssetId): F[Unit] =
      val merge = for
        _ <- sql"UPDATE ${AssetEntries} SET ${AssetEntries.assetId} = $targetId WHERE ${AssetEntries.assetId} = $sourceId"
          .update.run
        _ <- sql"""INSERT OR IGNORE INTO ${AssetsAuthors} (${AssetsAuthors.assetId}, ${AssetsAuthors.authorId})
              SELECT $targetId, ${AssetsAuthors.authorId} FROM ${AssetsAuthors} WHERE ${AssetsAuthors.assetId} = $sourceId"""
          .update.run
        _ <- sql"DELETE FROM ${Assets} WHERE ${Assets.id} = $sourceId"
          .update.run
      yield ()
      merge.transact(xa)

    override def matchCategoriesToAssets(
        categoryIds: NonEmptyList[CategoryId]
    ): F[Map[CategoryId, List[AssetId]]] =
      val query = sql"""
      SELECT ${Assets.id}, ${Assets.categoryId}
      FROM ${Assets}
      WHERE """ ++ Fragments.in(Assets.categoryId, categoryIds)
      query
        .queryOf(Columns(Assets.id, Assets.categoryId))
        .to[List]
        .transact(xa)
        .map: rows =>
          rows
            .groupBy(_._2)
            .foldLeft(Map.empty):
              case (acc, (Some(categoryId), group)) =>
                acc + (categoryId -> group.map(_._1))
              case (acc, _) => acc

    override def findOrAdd(assets: Set[NewAsset]): F[Set[ExistingAsset]] =
      NonEmptyList
        .fromList(assets.toList)
        .fold(Set.empty.pure): newAssets =>
          val titles        = newAssets.map(_.title)
          val assetsByTitle = newAssets.map(a => a.title -> a).toList.toMap
          val query = sql"""
          SELECT ${Assets.*}
          FROM ${Assets}
          WHERE """ ++ Fragments.in(Assets.title, titles)
          query
            .queryOf(Assets.*)
            .to[List]
            .transact(xa)
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

    private def findAsset(assetId: AssetId): F[Option[AssetModel]] =
      sql"""
        SELECT ${Assets.*}
        FROM ${Assets}
        WHERE ${Assets.id === assetId}
      """
        .queryOf(Assets.*)
        .option
        .transact(xa)
        .map: row =>
          row.map(Tuples.from[AssetModel](_))

    private def findAssetId(entryId: EntryId): F[Option[AssetId]] =
      sql"""
        SELECT ${AssetEntries.assetId}
        FROM ${AssetEntries}
        WHERE ${AssetEntries.id === entryId}
      """
        .queryOf(AssetEntries.assetId)
        .option
        .transact(xa)

    private def findEntries(assetId: AssetId): F[List[ExistingAssetEntry]] =
      sql"""
        SELECT ${AssetEntries.*} 
        FROM ${AssetEntries} 
        WHERE ${AssetEntries.assetId === assetId}
      """
        .queryOf(AssetEntries.*)
        .to[List]
        .transact(xa)
        .map: rows =>
          rows.map(Tuples.from[ExistingAssetEntry](_))

    private def addWithoutChecking(asset: NewAsset): F[AssetModel] =
      val insert = for
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
      insert.transact(xa)

    private def addWithoutChecking(
        entry: NewAssetEntry
    ): F[ExistingAssetEntry] =
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
        .transact(xa)
        .map(Tuples.from[ExistingAssetEntry](_))

    private def doesAssetExist(title: AssetTitle): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${Assets}
        WHERE ${Assets.title} = ${title}
      """.query[Int].option.transact(xa).map(_.isDefined)

    private def doesAssetExist(id: AssetId): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${Assets}
        WHERE ${Assets.id} = ${id}
      """.query[Int].option.transact(xa).map(_.isDefined)

    private def doesEntryExist(entryUri: EntryUri): F[Boolean] =
      sql"""
        SELECT 1
        FROM ${AssetEntries}
        WHERE ${AssetEntries.uri === entryUri}
      """.query[Int].option.transact(xa).map(_.isDefined)

    private def findAuthors(assetId: AssetId): F[List[AuthorId]] =
      sql"""
        SELECT ${AssetsAuthors.authorId}
        FROM ${AssetsAuthors}
        WHERE ${AssetsAuthors.assetId} = ${assetId}
      """.query[AuthorId].to[List].transact(xa)

    private def findAuthors(
        assetIds: List[AssetId]
    ): F[Map[AssetId, List[AuthorId]]] =
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
            .transact(xa)
            .map: results =>
              results.groupMap(_._2)(_._1)
        .getOrElse(Map.empty.pure)

private object Assets extends TableDefinition("assets"):
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

private object AssetsAuthors extends TableDefinition("assets_authors"):
  val assetId  = Column[AssetId]("asset_id")
  val authorId = Column[AuthorId]("author_id")
