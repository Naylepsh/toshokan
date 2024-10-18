package progressTracking
package assetMapping

import cats.Parallel
import cats.data.{EitherT, NonEmptyList, OptionT}
import cats.effect.*
import cats.syntax.all.*
import core.Tuples
import doobie.ConnectionIO
import doobie.implicits.*
import doobie.util.fragment.Fragment
import doobie.util.query.Query0
import doobie.util.transactor.Transactor
import doobiex.*
import library.AssetService
import library.category.CategoryService
import library.domain.*
import progressTracking.mal.{Manga as _, *}

import util.control.NoStackTrace
import domain.*

case object AssetNotFound extends NoStackTrace
type AssetNotFound = AssetNotFound.type

case object CategoryNotFound extends NoStackTrace
type CategoryNotFound = CategoryNotFound.type

case object AssetIsNotManga extends NoStackTrace
type AssetIsNotManga = AssetIsNotManga.type

case object ExternalIdAlreadyInUse extends NoStackTrace
type ExternalIdAlreadyInUse = ExternalIdAlreadyInUse.type

case object MangaAlreadyHasExternalIdAssigned extends NoStackTrace
type MangaAlreadyHasExternalIdAssigned = MangaAlreadyHasExternalIdAssigned.type

type AssignExternalIdToMangaError = AssetNotFound | CategoryNotFound |
  AssetIsNotManga | ExternalIdAlreadyInUse | MangaAlreadyHasExternalIdAssigned

type FindMalMappingError = AssetNotFound | CategoryNotFound | AssetIsNotManga

private type Result[A] = Either[Throwable, A]

class AssetMappingService[F[_]: Sync: Parallel](
    assetService: AssetService[F],
    categoryService: CategoryService[F],
    malService: MyAnimeListService[F],
    xa: Transactor[F]
):
  def searchForManga(term: Term): F[Either[Throwable, List[Manga]]] =
    malService.searchForManga(term)

  def findExternalId(asset: ExistingAsset): F[Option[ExternalMangaId]] =
    (for
      category <- OptionT(
        asset.categoryId.traverse(categoryService.find).map(_.flatten)
      )
      mangaId    <- OptionT.fromOption(MangaId(asset.id, category.name))
      externalId <- OptionT(MalMangaMappingSql.findMalId(mangaId).transact(xa))
    yield externalId).value

  def findAssetWithMalMapping(
      assetId: AssetId
  ): F[Either[FindMalMappingError, Option[
    (ExistingAsset, ExistingMalMangaMapping)
  ]]] =
    (for
      (asset, _) <- EitherT.fromOptionF(
        assetService.find(assetId),
        AssetNotFound: FindMalMappingError
      )
      categoryId <- EitherT.fromOption(asset.categoryId, CategoryNotFound)
      category <- EitherT.fromOptionF(
        categoryService.find(categoryId),
        CategoryNotFound
      )
      mangaId <- EitherT.fromOption(
        MangaId(asset.id, category.name),
        AssetIsNotManga
      )
      mapping <- EitherT.liftF(
        MalMangaMappingSql.findMapping(mangaId).transact(xa)
      )
    yield mapping.map(asset -> _)).value

  def deleteMapping(assetId: AssetId): F[Unit] =
    (for
      (asset, _) <- OptionT(assetService.find(assetId))
      categoryId <- OptionT.fromOption(asset.categoryId)
      category   <- OptionT(categoryService.find(categoryId))
      mangaId    <- OptionT.fromOption(MangaId(assetId, category.name))
      _ <- OptionT.liftF(MalMangaMappingSql.delete(mangaId).transact(xa))
    yield ()).value.void

  def assignExternalIdToManga(
      externalId: ExternalMangaId,
      internalId: AssetId
  ): F[Either[AssignExternalIdToMangaError, Unit]] =
    val getMangaId = for
      (asset, _) <- EitherT.fromOptionF(
        assetService.find(internalId),
        AssetNotFound: AssignExternalIdToMangaError
      )
      category <- EitherT.fromOptionF(
        asset.categoryId.traverse(categoryService.find).map(_.flatten),
        CategoryNotFound
      )
      mangaId <- EitherT.fromOption(
        MangaId(asset.id, category.name),
        AssetIsNotManga
      )
    yield mangaId

    getMangaId.value.flatMap:
      case Left(error) => error.asLeft.pure
      case Right(mangaId) =>
        (
          MalMangaMappingSql.findMalId(mangaId),
          MalMangaMappingSql.findMangaId(externalId)
        ).tupled
          .transact(xa)
          .flatMap:
            case (_, Some(_)) =>
              ExternalIdAlreadyInUse.asLeft.pure
            case (Some(_), _) =>
              MangaAlreadyHasExternalIdAssigned.asLeft.pure
            case (None, None) =>
              MalMangaMappingSql
                .assignMalIdToManga(externalId, mangaId)
                .transact(xa)
                .map(_.asRight)

private object MalMangaMapping extends TableDefinition("mal_manga_mapping"):
  val id      = Column[MalMangaMappingId]("id")
  val mangaId = Column[MangaId]("manga_id")
  val malId   = Column[ExternalMangaId]("mal_id")

  val * = Columns(id, mangaId, malId)

private object MalMangaMappingSql:
  def assignMalIdToManga(
      externalId: ExternalMangaId,
      internalId: MangaId
  ): ConnectionIO[Unit] =
    insertInto(
      MalMangaMapping,
      NonEmptyList.of(_.mangaId --> internalId, _.malId --> externalId)
    ).update.run.void

  def findMalId(
      mangaId: MangaId
  ): ConnectionIO[Option[ExternalMangaId]] =
    sql"""
    SELECT ${MalMangaMapping.malId}
    FROM ${MalMangaMapping}
    WHERE ${MalMangaMapping.mangaId === mangaId}"""
      .queryOf(MalMangaMapping.malId)
      .option

  def findMangaId(malId: ExternalMangaId): ConnectionIO[Option[MangaId]] =
    sql"""
    SELECT ${MalMangaMapping.mangaId}
    FROM ${MalMangaMapping}
    WHERE ${MalMangaMapping.malId === malId}"""
      .queryOf(MalMangaMapping.mangaId)
      .option

  def findMapping(
      mangaId: MangaId
  ): ConnectionIO[Option[ExistingMalMangaMapping]] =
    sql"""
    SELECT ${MalMangaMapping.*}
    FROM ${MalMangaMapping}
    WHERE ${MalMangaMapping.mangaId === mangaId}
    """
      .queryOf(MalMangaMapping.*)
      .option
      .map(_.map(Tuples.from[ExistingMalMangaMapping](_)))

  def delete(mangaId: MangaId) =
    sql"""
    DELETE FROM ${MalMangaMapping}
    WHERE ${MalMangaMapping.mangaId === mangaId}
    """.update.run.void
