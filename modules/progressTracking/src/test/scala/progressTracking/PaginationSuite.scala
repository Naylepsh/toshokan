package progressTracking
package viewComponents

class PaginationSuite extends munit.FunSuite:
  List(
    (
      "[|1|, 2, 3]",
      1,
      4,
      List("1", "2", "3")
    ),
    (
      "[1, |2|, 3, 4]",
      2,
      4,
      List("1", "2", "3", "4")
    ),
    (
      "[1, 2, |3|, 4, 5, ..., 42]",
      3,
      42,
      List("1", "2", "3", "4", "5", "...", "42")
    ),
    (
      "[1, ..., 5, 6, |7|, 8, 9, ..., 42]",
      7,
      42,
      List("1", "...", "5", "6", "7", "8", "9", "...", "42")
    ),
    (
      "[1, ..., 38, 39, |40|, 41, 42]",
      40,
      42,
      List("1", "...", "38", "39", "40", "41", "42")
    )
  ).foreach: (label, currentPage, lastPage, expected) =>
    test(label):
      val actual = Pagination(currentPage, lastPage).pages
      assertEquals(actual, expected)
