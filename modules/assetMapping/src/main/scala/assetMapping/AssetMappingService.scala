package assetMapping

import cats.MonadThrow
import cats.data.{EitherT, OptionT}
import cats.effect.*
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.util.transactor.Transactor
import library.asset.AssetService
import library.asset.domain.*
import library.category.CategoryService
import myAnimeList.MyAnimeListService
import myAnimeList.domain.{ExternalMangaId, Manga, Term}

import scala.util.control.NoStackTrace
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

class AssetMappingService[F[_]: MonadThrow: Sync](
    assetService: AssetService[F],
    categoryService: CategoryService[F],
    malService: MyAnimeListService[F],
    repo: MalMangaMappingRepository,
    xa: Transactor[F]
):
  def searchForManga(term: Term): F[List[Manga]] =
    malService.searchForManga(term)

  def findExternalId(asset: ExistingAsset): F[Option[ExternalMangaId]] =
    (for
      category <- OptionT(
        asset.categoryId.traverse(categoryService.find).map(_.flatten)
      )
      mangaId    <- OptionT.fromOption(MangaId(asset.id, category.name))
      externalId <- OptionT(repo.findMalId(mangaId).transact(xa))
    yield externalId).value

  def findAssetWithMalMapping(
      assetId: AssetId
  ): Raise[F, FindMalMappingError] ?=> F[Option[
    (ExistingAsset, ExistingMalMangaMapping)
  ]] =
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
        repo.findMapping(mangaId).transact(xa)
      )
    yield mapping.map(asset -> _)).value.flatMap:
      case Left(error)   => error.raise
      case Right(result) => result.pure

  def deleteMapping(assetId: AssetId): F[Unit] =
    (for
      (asset, _) <- OptionT(assetService.find(assetId))
      categoryId <- OptionT.fromOption(asset.categoryId)
      category   <- OptionT(categoryService.find(categoryId))
      mangaId    <- OptionT.fromOption(MangaId(assetId, category.name))
      _ <- OptionT.liftF(repo.delete(mangaId).transact(xa))
    yield ()).value.void

  def assignExternalIdToManga(
      externalId: ExternalMangaId,
      internalId: AssetId
  ): Raise[F, AssignExternalIdToMangaError] ?=> F[Unit] =
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
      case Left(error) => error.raise
      case Right(mangaId) =>
        (
          repo.findMalId(mangaId),
          repo.findMangaId(externalId)
        ).tupled
          .transact(xa)
          .flatMap:
            case (_, Some(_)) =>
              ExternalIdAlreadyInUse.raise
            case (Some(_), _) =>
              MangaAlreadyHasExternalIdAssigned.raise
            case (None, None) =>
              repo
                .assignMalIdToManga(externalId, mangaId)
                .transact(xa)
