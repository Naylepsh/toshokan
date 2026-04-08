package library.asset

import library.asset.domain.ExistingAsset

case class AssetGroup(normalizedTitle: String, assets: List[ExistingAsset]):
  def isMergeSuggestion: Boolean = assets.size > 1

object AssetGroup:
  def fromAssets(assets: List[ExistingAsset]): List[AssetGroup] =
    val (magazines, groupable) = assets.partition(a => TitleNormalizer.isMagazineTitle(a.title))

    val exactGroups = groupable
      .groupBy(a => TitleNormalizer.normalize(a.title))
      .map((normalized, group) => AssetGroup(normalized, group))
      .toList

    val (multi, singles) = exactGroups.partition(_.isMergeSuggestion)
    val fuzzyGroups      = mergeSimilarGroups(singles)
    val magazineGroups   = magazines.map(a => AssetGroup(TitleNormalizer.normalize(a.title), List(a)))

    (multi ++ fuzzyGroups ++ magazineGroups).sortBy(_.normalizedTitle)

  private def mergeSimilarGroups(
      groups: List[AssetGroup]
  ): List[AssetGroup] =
    val used    = scala.collection.mutable.Set.empty[Int]
    val result  = scala.collection.mutable.ListBuffer.empty[AssetGroup]
    val indexed = groups.zipWithIndex
    for (group, i) <- indexed if !used.contains(i) do
      val similar = indexed.collect:
        case (other, j)
            if j > i && !used.contains(j) &&
              TitleNormalizer.areSimilar(
                group.normalizedTitle,
                other.normalizedTitle
              ) =>
          used += j
          other
      if similar.nonEmpty then
        used += i
        result += AssetGroup(
          group.normalizedTitle,
          group.assets ++ similar.flatMap(_.assets)
        )
      else result += group
    result.toList
