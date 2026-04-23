package library.asset.domain

object TitleNormalizer:
  private val altTitleSeparator = "\\s*\\|.*$".r
  private val parenthetical     = "\\s*\\([^)]*\\)\\s*$".r
  private val trailingNumber    = "\\s+(\\d+)\\s*$".r
  private val volumeChapter =
    "(?i)\\s*(vol\\.?|ch\\.?|chapter|part|#)\\s*\\d+\\s*$".r
  private val trailingSono      = "(?i)\\s+sono\\s+\\d+\\s*$".r
  private val commonPunctuation = "[.,\\-~!?:;'\"]+".r
  private val multipleSpaces    = "\\s+".r

  def normalize(title: String): String =
    var t = title
    t = altTitleSeparator.replaceFirstIn(t, "")
    t = parenthetical.replaceFirstIn(t, "")
    t = volumeChapter.replaceFirstIn(t, "")
    t = trailingSono.replaceFirstIn(t, "")
    t = trailingNumber.replaceFirstIn(t, "")
    t = t.toLowerCase.trim
    t = commonPunctuation.replaceAllIn(t, "")
    t = multipleSpaces.replaceAllIn(t, " ").trim
    t

  private val minLengthForFuzzy = 8

  private val magazinePatterns = List(
    "(?i)comic\\s+.+\\d{4}".r,
    "\\d{4}年.*月号".r,
    "\\d{4}年.*月.*fanbox".r,
    "(?i)\\d{4}-\\d{2}".r
  )

  def isMagazineTitle(title: String): Boolean =
    val lower = title.toLowerCase
    magazinePatterns.exists(_.findFirstIn(lower).isDefined)

  def areSimilar(a: String, b: String): Boolean =
    val maxLen = math.max(a.length, b.length)
    if maxLen < minLengthForFuzzy then false
    else
      val dist      = core.StringDistance.levenshtein(a, b)
      val threshold = math.max(1, (maxLen * 0.15).toInt)
      dist <= threshold
