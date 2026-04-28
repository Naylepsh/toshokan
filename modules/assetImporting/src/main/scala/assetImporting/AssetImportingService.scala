package assetImporting

import assetImporting.domain.*
import assetMapping.{AssetMappingService, AssignExternalIdToMangaError}
import assetScraping.configs.AssetScrapingConfigService
import assetScraping.configs.domain.*
import cats.effect.IO
import cats.mtl.Handle
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import library.asset.AssetService
import library.asset.domain.*
import library.author.AuthorRepository
import library.author.domain.AuthorName
import library.category.CategoryService
import library.category.domain.{CategoryDoesNotExist, ExistingCategory}
import mangadex.MangadexApi
import mangadex.schemas.manga.GetMangaResponse
import myAnimeList.domain.ExternalMangaId

import domain.MangadexMangaUri

class AssetImportingService(
    assetService: AssetService,
    categoryService: CategoryService,
    assetMappingService: AssetMappingService,
    assetScrapingConfigService: AssetScrapingConfigService,
    authorRepository: AuthorRepository,
    mangadex: MangadexApi,
    xa: Transactor[IO]
):
  def importFromMangadex(uri: MangadexMangaUri): IO[ExistingAsset] =
    categoryService.findManga.flatMap:
      case None =>
        IO.raiseError(CategoryDoesNotExist)
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
        .liftTo[IO](new RuntimeException(NoTitleTranslation.toString))
      authors <- authorRepository
        .findOrAdd(
          mangaResponse.data.authorNames.map(AuthorName(_)).toSet
        )
        .transact(xa)
      result <-
        Handle
          .allow[AddAssetError]:
            assetService.add(
              NewAsset(
                AssetTitle(title),
                manga.id.some,
                authors.map(_.id).toList
              )
            )
          .rescue: error =>
            IO.raiseError(new RuntimeException(error.toString))
    yield result

  private def createScrapingConfig(
      asset: ExistingAsset,
      uri: MangadexMangaUri
  ): IO[Unit] =
    for
      config <- NewAssetScrapingConfig(
        ScrapingConfigUri(uri),
        AssetSite.Mangadex,
        IsConfigEnabled(true),
        asset.id
      ).leftMap(error => new RuntimeException(error.toString)).liftTo[IO]
      result <-
        Handle
          .allow[AddScrapingConfigError]:
            assetScrapingConfigService.add(config).void
          .rescue:
            case error: AddScrapingConfigError =>
              IO.raiseError(new RuntimeException(error.toString))
    yield result

  private def assignExternalIdToManga(
      asset: ExistingAsset,
      malId: ExternalMangaId
  ) = Handle
    .allow[AssignExternalIdToMangaError]:
      assetMappingService.assignExternalIdToManga(malId, asset.id)
    .rescue:
      case error => IO.raiseError(error)
