package authorMerging

import assetMapping.MalMangaMappingRepository
import assetScraping.configs.{
  AssetScrapingConfigRepository,
  AuthorScrapingConfigRepository
}
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import library.asset.AssetRepository
import library.asset.domain.AssetId
import library.author.AuthorRepository
import library.author.domain.AuthorId

class AuthorMergeService(
    authorRepo: AuthorRepository,
    assetRepo: AssetRepository,
    authorScrapingConfigRepo: AuthorScrapingConfigRepository,
    assetScrapingConfigRepo: AssetScrapingConfigRepository,
    malMangaMappingRepo: MalMangaMappingRepository,
    xa: Transactor[IO]
):
  def mergeAuthors(
      sourceIds: NonEmptyList[AuthorId],
      targetId: AuthorId
  ): IO[Unit] =
    val merge = for
      _ <- authorScrapingConfigRepo.transferConfigs(sourceIds, targetId)
      targetAssets <- assetRepo.findAssetsByAuthor(targetId)
      targetAssetsByTitle = targetAssets.map((id, title) => title -> id).toMap
      sourceAssets <- sourceIds.toList.flatTraverse: sourceId =>
        assetRepo.findAssetsByAuthor(sourceId)
      distinctSourceAssets = sourceAssets.distinctBy(_._1)
      _ <- distinctSourceAssets.traverse_ : (sourceAssetId, title) =>
        targetAssetsByTitle.get(title) match
          case Some(targetAssetId) if targetAssetId != sourceAssetId =>
            mergeAssets(sourceAssetId, targetAssetId)
          case _ =>
            assetRepo.relinkAuthorAssets(sourceAssetId, targetId)
      _ <- authorRepo.recordAliases(sourceIds, targetId)
      _ <- authorRepo.deleteAuthors(sourceIds)
    yield ()
    merge.transact(xa)

  private def mergeAssets(source: AssetId, target: AssetId) =
    for
      _ <- assetScrapingConfigRepo.transferConfigs(source, target)
      _ <- malMangaMappingRepo.transferMappings(source, target)
      _ <- assetRepo.mergeAsset(source, target)
    yield ()
