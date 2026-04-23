package core

object StringDistance:
  def levenshtein(s: String, t: String): Int =
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
