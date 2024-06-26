package doobiex

import cats.data.NonEmptyList
import doobie.*

class FragmentsSuite extends munit.FunSuite:
  import FragmentsSuite.*

  test("insertInto"):
    val query =
      insertInto(
        Person,
        NonEmptyList.of(_.firstName --> "John", _.age --> 42)
      ).update
    assertEquals(
      normalize(query.sql),
      """INSERT INTO people (first_name, age) VALUES (?, ?)"""
    )

  test("insertIntoReturning"):
    val query =
      insertIntoReturning(
        Person,
        NonEmptyList.of(_.firstName --> "John", _.age --> 42),
        _.*
      ).update
    assertEquals(
      normalize(query.sql),
      """INSERT INTO people (first_name, age) VALUES (?, ?) RETURNING id, first_name, age"""
    )

  test("updateTable"):
    val query =
      updateTable(
        Person,
        NonEmptyList.of(_.firstName --> "John", _.age --> 42)
      ).update
    assertEquals(
      normalize(query.sql),
      """UPDATE people SET first_name=?, age=?"""
    )

object FragmentsSuite:
  object Person extends TableDefinition("people"):
    val id        = Column[Long]("id")
    val firstName = Column[String]("first_name")
    val age       = Column[Int]("age")

    val * = Columns((id, firstName, age))

  def normalize(str: String): String = str.replaceAll("[ ]+", " ").trim
