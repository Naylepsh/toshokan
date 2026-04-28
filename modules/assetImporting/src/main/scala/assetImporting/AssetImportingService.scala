package assetImporting

import assetImporting.domain.*
import assetMapping.{AssetMappingService, AssignExternalIdToMangaError}
import assetScraping.configs.AssetScrapingConfigService
import assetScraping.configs.domain.*
import cats.effect.IO
import cats.mtl.syntax.all.*
import cats.mtl.{Handle, Raise}
import cats.syntax.all.*
import core.syntax.*
import doobie.*
import doobie.implicits.*
import library.asset.AssetService
import library.asset.domain.*
import library.author.AuthorRepository
import library.author.domain.AuthorName
import library.category.CategoryService
import library.category.domain.ExistingCategory
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
  def importFromMangadex(
      uri: MangadexMangaUri
  ): Raise[IO, ImportError] ?=> IO[ExistingAsset] =
    for
      manga <- categoryService.findManga.someOrRaise(
        ImportError.CategoryDoesNotExist
      )
      mangaResponse <- getMangaFromMangadex(uri.id)
      malId = extractMalId(mangaResponse)
      createdAsset <- createAsset(mangaResponse, manga)
      _            <- createScrapingConfig(createdAsset, uri)
      _            <- malId.traverse(assignExternalIdToManga(createdAsset, _))
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
  )(using Raise[IO, ImportError]) =
    for
      title <- mangaResponse.data.attributes.preferredTitle
        .orRaise(ImportError.NoTitleTranslation)
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
          .rescue: _ =>
            ImportError.AssetAlreadyExists.raise
    yield result

  private def createScrapingConfig(
      asset: ExistingAsset,
      uri: MangadexMangaUri
  )(using Raise[IO, ImportError]): IO[Unit] =
    for
      config <- NewAssetScrapingConfig(
        ScrapingConfigUri(uri),
        AssetSite.Mangadex,
        IsConfigEnabled(true),
        asset.id
      ) match
        case Right(c)    => IO.pure(c)
        case Left(error) => ImportError.ScrapingConfigError(error).raise
      _ <-
        Handle
          .allow[AddScrapingConfigError]:
            assetScrapingConfigService.add(config).void
          .rescue: error =>
            ImportError.ScrapingConfigError(error.toString).raise
    yield ()

  private def assignExternalIdToManga(
      asset: ExistingAsset,
      malId: ExternalMangaId
  )(using Raise[IO, ImportError]) = Handle
    .allow[AssignExternalIdToMangaError]:
      assetMappingService.assignExternalIdToManga(malId, asset.id)
    .rescue: error =>
      ImportError.MappingError(error.toString).raise
