package assetImporting

import assetImporting.domain.{CategoryDoesNotExist, NoMalIdAvailable}
import assetMapping.AssetMappingService
import assetScraping.configs.AssetScrapingConfigService
import assetScraping.configs.domain.*
import cats.data.EitherT
import cats.syntax.all.*
import cats.{Monad, Parallel}
import library.AssetService
import library.category.CategoryService
import library.category.domain.ExistingCategory
import library.domain.{AssetTitle, ExistingAsset, NewAsset}
import mangadex.MangadexApi
import mangadex.schemas.manga.GetMangaResponse
import myAnimeList.domain.ExternalMangaId

import domain.MangadexMangaUri
import assetImporting.domain.MangadexId

class AssetImportingService[F[_]: Monad: Parallel](
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
          malId         <- extractMalId(mangaResponse)
          createdAsset  <- createAsset(mangaResponse, manga)
          _             <- createScrapingConfig(createdAsset, uri)
          _             <- assignExternalIdToManga(createdAsset, malId)
        yield createdAsset).value

  private def getMangaFromMangadex(id: MangadexId) =
    EitherT(mangadex.getManga(id.toString))

  private def extractMalId(mangaResponse: GetMangaResponse) =
    EitherT
      .fromOption(
        mangaResponse.data.attributes.links.mal.flatMap(_.toLongOption),
        NoMalIdAvailable
      )
      .map(ExternalMangaId.apply)

  private def createAsset(
      mangaResponse: GetMangaResponse,
      manga: ExistingCategory
  ) =
    EitherT(
      assetService.add(
        NewAsset(
          AssetTitle(mangaResponse.data.attributes.title.en),
          manga.id.some
        )
      )
    )

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
