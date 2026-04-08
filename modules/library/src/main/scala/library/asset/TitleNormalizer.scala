package library.asset

object TitleNormalizer:
  private val altTitleSeparator = "\\s*\\|.*$".r
  private val parenthetical    = "\\s*\\([^)]*\\)\\s*$".r
  private val trailingNumber   = "\\s+(\\d+)\\s*$".r
  private val volumeChapter =
    "(?i)\\s*(vol\\.?|ch\\.?|chapter|part|#)\\s*\\d+\\s*$".r
  private val trailingSono     = "(?i)\\s+sono\\s+\\d+\\s*$".r
  private val commonPunctuation = "[.,\\-~!?:;'\"]+".r
  private val multipleSpaces   = "\\s+".r

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
      val dist      = levenshtein(a, b)
      val threshold = math.max(1, (maxLen * 0.15).toInt)
      dist <= threshold

  private def levenshtein(s: String, t: String): Int =
    val m = s.length
    val n = t.length
    val d = Array.ofDim[Int](m + 1, n + 1)
    for i <- 0 to m do d(i)(0) = i
    for j <- 0 to n do d(0)(j) = j
    for
      i <- 1 to m
      j <- 1 to n
    do
      val cost = if s(i - 1) == t(j - 1) then 0 else 1
      d(i)(j) = math.min(
        math.min(d(i - 1)(j) + 1, d(i)(j - 1) + 1),
        d(i - 1)(j - 1) + cost
      )
    d(m)(n)
