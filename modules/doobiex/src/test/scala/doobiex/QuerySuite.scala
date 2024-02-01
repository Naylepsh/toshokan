package doobiex

import doobie.*
import doobie.implicits.*

class QuerySuite extends munit.FunSuite:
  import QuerySuite.*

  test("select particular column"):
    val expected = "SELECT id FROM people"
    val actual   = sql"SELECT ${Person.id} FROM ${Person}".queryOf(Person.id).sql

    assertEquals(normalize(actual), expected)

  test("select all columns"):
    val expected = "SELECT id, first_name, age FROM people"
    val actual   = sql"SELECT ${Person.*} FROM ${Person}".queryOf(Person.*).sql

    assertEquals(normalize(actual), expected)

  // TODO: Required more work
  // test("select columns from aliased table"):
  //   val P = Person as "p"
  //   val expected = "SELECT id, first_name, age FROM people AS p"
  //   val actual   = sql"SELECT ${P.*} FROM ${P}".queryOf(P.*).sql
  //
  //   assertEquals(normalize(actual), expected)

object QuerySuite:
  object Person extends TableDefinition("people"):
    val id        = Column[Long]("id")
    val firstName = Column[String]("first_name")
    val age       = Column[Int]("age")

    val * = Columns((id, firstName, age))

  def normalize(str: String): String = str.replaceAll("[ ]+", " ").trim
