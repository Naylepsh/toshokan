package library.asset

class TitleNormalizerSuite extends munit.FunSuite:
  import TitleNormalizer.normalize

  test("strips alternative title after |"):
    assertEquals(
      normalize("Foo Bar | Some English Title"),
      normalize("Foo Bar")
    )

  test("strips parenthetical suffixes"):
    assertEquals(normalize("Foo Bar (decensored)"), normalize("Foo Bar"))
    assertEquals(normalize("Foo Bar (english)"), normalize("Foo Bar"))
    assertEquals(normalize("Foo Bar (uncensored)"), normalize("Foo Bar"))

  test("strips trailing numbers"):
    assertEquals(normalize("Foo Bar 1"), normalize("Foo Bar"))
    assertEquals(normalize("Foo Bar 2"), normalize("Foo Bar"))

  test("strips volume/chapter indicators"):
    assertEquals(normalize("Foo Bar Vol. 3"), normalize("Foo Bar"))
    assertEquals(normalize("Foo Bar Ch.2"), normalize("Foo Bar"))
    assertEquals(normalize("Foo Bar #4"), normalize("Foo Bar"))
    assertEquals(normalize("Foo Bar Part 1"), normalize("Foo Bar"))

  test("strips trailing 'Sono N'"):
    assertEquals(
      normalize("Kanojo to Game suru Hanashi Sono 2"),
      normalize("Kanojo to Game suru Hanashi")
    )

  test("case insensitive"):
    assertEquals(
      normalize("Danshi o Mukuna mama"),
      normalize("Danshi o mukuna mama")
    )

  test("ignores punctuation differences"):
    assertEquals(
      normalize("Misetsuke. Rikujoubu"),
      normalize("Misetsuke Rikujoubu")
    )

  test("combined: alt title + parenthetical + number"):
    assertEquals(
      normalize("Foo 3 (decensored) | Bar Baz"),
      normalize("Foo")
    )

  test("does not strip numbers that are part of the title"):
    assertNotEquals(normalize("3P"), normalize(""))
    assertEquals(normalize("3P"), "3p")

  test("areSimilar: small spacing difference"):
    assert(TitleNormalizer.areSimilar(
      normalize("Danshi o Muku na mama Sodatete"),
      normalize("Danshi o Mukuna mama Sodatete")
    ))

  test("areSimilar: completely different titles"):
    assert(!TitleNormalizer.areSimilar(
      normalize("Foo Bar Baz Qux Quux"),
      normalize("Something Else Entirely Different")
    ))

  test("areSimilar: short titles require exact match"):
    assert(!TitleNormalizer.areSimilar("abc", "abd"))
    assert(!TitleNormalizer.areSimilar("0920", "0926"))
    assert(!TitleNormalizer.areSimilar("山城", "葛城"))

  test("isMagazineTitle: detects magazine patterns"):
    assert(TitleNormalizer.isMagazineTitle("COMIC アンスリウム 2024年12月号"))
    assert(TitleNormalizer.isMagazineTitle("COMIC BAVEL 2022-02 (decensored)"))
    assert(TitleNormalizer.isMagazineTitle("コミックホットミルク 2025年6月号"))
    assert(TitleNormalizer.isMagazineTitle("2024年9月Fanbox合集"))

  test("isMagazineTitle: does not match regular titles"):
    assert(!TitleNormalizer.isMagazineTitle("Foo Bar Baz"))
    assert(!TitleNormalizer.isMagazineTitle("Yokubou Pandora 29"))
