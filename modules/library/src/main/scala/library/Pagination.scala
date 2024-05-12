package library

case class Pagination(current: Int, last: Int):
  import Pagination.*

  /** Expected behavior:
    *   - [1,2,3]
    *   - [1,2,3, ..., 42]
    *   - [1, ..., 10, 11, 12, ..., 42]
    *   - [1, ..., 40, 41, 42]
    */
  val pages =
    val core = ((current - offset).max(1) to (current + offset).min(last))
      .map(_.toString)
      .toList
    val start =
      if 1 + offset < current
      then List("1", skipped)
      else Nil
    val `end` =
      if current < last - offset - 1
      then List(skipped, last.toString)
      else Nil
    start ++ core ++ `end`

object Pagination:
  val skipped = "..."

  private val offset = 2

  private val itemsPerPage = 20
  def paginate[T](xs: List[T], page: Int): (List[T], Pagination) =
    val pagination = Pagination(
      current = page,
      last = math.ceil(xs.length.toFloat / itemsPerPage).toInt
    )
    val items = xs.drop(itemsPerPage * (page - 1)).take(itemsPerPage)
    (items, pagination)
