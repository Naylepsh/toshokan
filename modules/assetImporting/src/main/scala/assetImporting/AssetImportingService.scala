package assetImporting

import assetImporting.domain.*
import assetMapping.AssetMappingService
import assetScraping.configs.AssetScrapingConfigService
import assetScraping.configs.domain.*
import cats.data.EitherT
import cats.syntax.all.*
import cats.Monad
import library.AssetService
import library.category.CategoryService
import library.category.domain.ExistingCategory
import library.domain.{AssetTitle, ExistingAsset, NewAsset}
import mangadex.MangadexApi
import mangadex.schemas.manga.GetMangaResponse
import myAnimeList.domain.ExternalMangaId

import domain.MangadexMangaUri

class AssetImportingService[F[_]: Monad](
    assetService: AssetService[F],
    categoryService: CategoryService[F],
    assetMappingService: AssetMappingService[F],
    assetScrapingConfigService: AssetScrapingConfigService[F],
    mangadex: MangadexApi[F]
):
  def importFromMangadex(
      uri: MangadexMangaUri
  ): F[Either[Throwable, ExistingAsset]] =
    categoryService.findManga.flatMap:
      case None => CategoryDoesNotExist.asLeft.pure
      case Some(manga) =>
        (for
          mangaResponse <- getMangaFromMangadex(uri.id)
          malId = extractMalId(mangaResponse)
          createdAsset <- createAsset(mangaResponse, manga)
          _            <- createScrapingConfig(createdAsset, uri)
          _ <- malId.traverse(assignExternalIdToManga(createdAsset, _))
        yield createdAsset).value

  private def getMangaFromMangadex(id: MangadexId) =
    EitherT(mangadex.getManga(id.toString))

  private def extractMalId(mangaResponse: GetMangaResponse) =
    mangaResponse.data.attributes.links.mal
      .flatMap(_.toLongOption)
      .map(ExternalMangaId.apply)

  private def createAsset(
      mangaResponse: GetMangaResponse,
      manga: ExistingCategory
  ): EitherT[F, Throwable, ExistingAsset] =
    for
      title <- EitherT.fromOption(
        mangaResponse.data.attributes.title.preferred,
        NoTitleTranslation
      )
      result <- EitherT(
        assetService
          .add(NewAsset(AssetTitle(title), manga.id.some))
          .map(_.leftWiden)
      )
    yield result

  private def createScrapingConfig(
      asset: ExistingAsset,
      uri: MangadexMangaUri
  ) =
    EitherT
      .fromEither(
        NewAssetScrapingConfig(
          ScrapingConfigUri(uri),
          Site.Mangadex,
          IsConfigEnabled(true),
          asset.id
        ).leftMap(new RuntimeException(_))
      )
      .flatMap: config =>
        EitherT(
          assetScrapingConfigService
            .add(config)
            .map(_.leftMap(error => new RuntimeException(error.toString)))
        )

  private def assignExternalIdToManga(
      asset: ExistingAsset,
      malId: ExternalMangaId
  ): EitherT[F, Throwable, Unit] =
    EitherT(
      assetMappingService
        .assignExternalIdToManga(malId, asset.id)
        .map(_.void)
    )
