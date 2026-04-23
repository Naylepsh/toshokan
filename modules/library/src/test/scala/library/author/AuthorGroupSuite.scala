package library.author

import library.author.domain.*

class AuthorGroupSuite extends munit.FunSuite:
  private def author(id: Long, name: String) =
    ExistingAuthor(AuthorId(id), AuthorName(name))

  test("groups authors with same name different casing"):
    val authors = List(
      author(1, "John Smith"),
      author(2, "john smith"),
      author(3, "JOHN SMITH")
    )
    val groups      = AuthorGroup.fromAuthors(authors)
    val suggestions = groups.filter(_.isMergeSuggestion)
    assertEquals(suggestions.size, 1)
    assertEquals(suggestions.head.authors.size, 3)

  test("does not group distinct names"):
    val authors = List(author(1, "Alice"), author(2, "Bob"))
    val groups  = AuthorGroup.fromAuthors(authors)
    assert(groups.forall(!_.isMergeSuggestion))

  test("groups authors differing only by punctuation"):
    val authors = List(
      author(1, "O'Brien"),
      author(2, "OBrien")
    )
    val groups      = AuthorGroup.fromAuthors(authors)
    val suggestions = groups.filter(_.isMergeSuggestion)
    assertEquals(suggestions.size, 1)

  test("groups authors differing only by extra spaces"):
    val authors = List(
      author(1, "John  Smith"),
      author(2, "John Smith")
    )
    val groups      = AuthorGroup.fromAuthors(authors)
    val suggestions = groups.filter(_.isMergeSuggestion)
    assertEquals(suggestions.size, 1)

  test("fuzzy groups similar long names"):
    val authors = List(
      author(1, "takemura sesshu"),
      author(2, "takemura seshu")
    )
    val groups      = AuthorGroup.fromAuthors(authors)
    val suggestions = groups.filter(_.isMergeSuggestion)
    assertEquals(suggestions.size, 1)

  test("does not fuzzy group short distinct names"):
    val authors = List(author(1, "abc"), author(2, "abd"))
    val groups  = AuthorGroup.fromAuthors(authors)
    assert(groups.forall(!_.isMergeSuggestion))
