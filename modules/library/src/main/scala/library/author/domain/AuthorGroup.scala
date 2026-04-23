package library.author.domain

import util.chaining.*

case class AuthorGroup(normalizedName: String, authors: List[ExistingAuthor]):
  def isMergeSuggestion: Boolean = authors.size > 1

object AuthorGroup:
  private val commonPunctuation = "[.,\\-~!?:;'\"]+".r
  private val multipleSpaces    = "\\s+".r

  def normalizeName(name: String): String =
    name.toLowerCase.trim
      .pipe(commonPunctuation.replaceAllIn(_, ""))
      .pipe(multipleSpaces.replaceAllIn(_, " "))
      .trim

  def fromAuthors(authors: List[ExistingAuthor]): List[AuthorGroup] =
    val exactGroups = authors
      .groupBy(a => normalizeName(a.name))
      .map(AuthorGroup.apply)
      .toList

    val (multi, singles) = exactGroups.partition(_.isMergeSuggestion)
    val fuzzyGroups      = mergeSimilarGroups(singles)

    (multi ++ fuzzyGroups).sortBy(_.normalizedName)

  private def mergeSimilarGroups(
      groups: List[AuthorGroup]
  ): List[AuthorGroup] =
    val used    = scala.collection.mutable.Set.empty[Int]
    val result  = scala.collection.mutable.ListBuffer.empty[AuthorGroup]
    val indexed = groups.zipWithIndex
    for (group, i) <- indexed if !used.contains(i) do
      val similar = indexed.collect:
        case (other, j)
            if j > i && !used.contains(j) &&
              areSimilar(group.normalizedName, other.normalizedName) =>
          used += j
          other
      if similar.nonEmpty then
        used += i
        result += AuthorGroup(
          group.normalizedName,
          group.authors ++ similar.flatMap(_.authors)
        )
      else result += group
    result.toList

  private val minLengthForFuzzy = 8

  private def areSimilar(a: String, b: String): Boolean =
    val maxLen = math.max(a.length, b.length)
    if maxLen < minLengthForFuzzy then false
    else
      val dist      = core.StringDistance.levenshtein(a, b)
      val threshold = math.max(1, (maxLen * 0.15).toInt)
      dist <= threshold
