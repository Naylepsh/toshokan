package assetImporting

import assetImporting.domain.*
import assetMapping.{AssetMappingService, AssignExternalIdToMangaError}
import assetScraping.configs.AssetScrapingConfigService
import assetScraping.configs.domain.*
import cats.effect.MonadCancelThrow
import cats.mtl.Handle
import cats.syntax.all.*
import library.AssetService
import library.category.CategoryService
import library.category.domain.ExistingCategory
import library.domain.*
import mangadex.MangadexApi
import mangadex.schemas.manga.GetMangaResponse
import myAnimeList.domain.ExternalMangaId

import domain.MangadexMangaUri

class AssetImportingService[F[_]: MonadCancelThrow](
    assetService: AssetService[F],
    categoryService: CategoryService[F],
    assetMappingService: AssetMappingService[F],
    assetScrapingConfigService: AssetScrapingConfigService[F],
    mangadex: MangadexApi[F]
):
  def importFromMangadex(
      uri: MangadexMangaUri
  ): F[ExistingAsset] =
    // TODO: Handle domain errors?
    categoryService.findManga.flatMap:
      case None =>
        MonadCancelThrow[F].raiseError(
          new RuntimeException(CategoryDoesNotExist.toString)
        )
      case Some(manga) =>
        for
          mangaResponse <- getMangaFromMangadex(uri.id)
          malId = extractMalId(mangaResponse)
          createdAsset <- createAsset(mangaResponse, manga)
          _            <- createScrapingConfig(createdAsset, uri)
          _ <- malId.traverse(assignExternalIdToManga(createdAsset, _))
        yield createdAsset

  private def getMangaFromMangadex(id: MangadexId) =
    mangadex.getManga(id.toString).rethrow

  private def extractMalId(mangaResponse: GetMangaResponse) =
    mangaResponse.data.attributes.links.mal
      .flatMap(_.toLongOption)
      .map(ExternalMangaId.apply)

  private def createAsset(
      mangaResponse: GetMangaResponse,
      manga: ExistingCategory
  ) =
    for
      title <- mangaResponse.data.attributes.preferredTitle
        .liftTo[F](new RuntimeException(NoTitleTranslation.toString))
      result <-
        Handle
          .allow[AddAssetError]:
            assetService
              .add(NewAsset(AssetTitle(title), manga.id.some))
          .rescue: error =>
            MonadCancelThrow[F].raiseError(new RuntimeException(error.toString))
    yield result

  private def createScrapingConfig(
      asset: ExistingAsset,
      uri: MangadexMangaUri
  ): F[Unit] =
    for
      config <- NewAssetScrapingConfig(
        ScrapingConfigUri(uri),
        Site.Mangadex,
        IsConfigEnabled(true),
        asset.id
      ).leftMap(error => new RuntimeException(error.toString)).liftTo[F]
      result <-
        Handle
          .allow[AddScrapingConfigError]:
            assetScrapingConfigService.add(config).void
          .rescue:
            case error: AddScrapingConfigError =>
              MonadCancelThrow[F].raiseError(
                new RuntimeException(error.toString)
              )
    yield result

  private def assignExternalIdToManga(
      asset: ExistingAsset,
      malId: ExternalMangaId
  ) = Handle
    .allow[AssignExternalIdToMangaError]:
      assetMappingService.assignExternalIdToManga(malId, asset.id)
    .rescue:
      case error => MonadCancelThrow[F].raiseError(error)
