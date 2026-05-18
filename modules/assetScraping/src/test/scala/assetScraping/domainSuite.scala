package assetScraping

import java.net.URI

import cats.syntax.all.*
import library.asset.domain.AssetId

import configs.domain.*

class domainSuite extends munit.FunSuite:
  List(
    (
      "Mismatched uri",
      AssetSite.Mangakakalot,
      "https://mangadex.org/title/296cbc31-af1a-4b5b-a34b-fee2b4cad542/-oshi-no-ko"
    ),
    (
      "Mismatched uri",
      AssetSite.Mangadex,
      "https://chapmanganato.to/manga-ku987903"
    ),
    (
      "Missing manga id",
      AssetSite.Mangadex,
      "https://mangadex.org/title"
    )
  ).foreach: (label, site, uri) =>
    test(label):
      val config = NewAssetScrapingConfig(
        ScrapingConfigUri(URI(uri)),
        site,
        IsConfigEnabled(true),
        AssetId(42)
      )
      assert(config.isLeft, s"Creation of config: $config did not fail")

  test("Mangadex uri normalization"):
    val uris = List(
      "https://mangadex.org/title/296cbc31-af1a-4b5b-a34b-fee2b4cad542/-oshi-no-ko",
      "https://mangadex.org/title/296cbc31-af1a-4b5b-a34b-fee2b4cad542/",
      "https://mangadex.org/title/296cbc31-af1a-4b5b-a34b-fee2b4cad542"
    )
    val configs = uris.traverse: uri =>
      NewAssetScrapingConfig(
        ScrapingConfigUri(URI(uri)),
        AssetSite.Mangadex,
        IsConfigEnabled(true),
        AssetId(42)
      )
    configs match
      case Left(error) =>
        assert(false, s"At least one config creation failed due to $error")
      case Right(configs) =>
        assertEquals(configs.map(_.uri).toSet.size, 1)

  test("Batoto uri normalization"):
    val config = NewAssetScrapingConfig(
      ScrapingConfigUri(
        URI(
          "https://bato.si/title/161599-the-maid-s-three-star-cuisine-in-another-world-i-used-real-life-dishes-to-become-a-palace-sensation"
        )
      ),
      AssetSite.Batoto,
      IsConfigEnabled(true),
      AssetId(42)
    )
    config match
      case Left(error) => assert(false, s"Config creation failed: $error")
      case Right(config) =>
        assertEquals(config.uri.toString, "https://bato.si/title/161599")

  test("AssetSite and AuthorSite cover all known sites"):
    val allSiteNames =
      AssetSite.values.map(_.toString).toSet ++
        AuthorSite.values.map(_.toString).toSet
    assertEquals(
      allSiteNames,
      Set(
        "Mangadex",
        "Mangakakalot",
        "Yatta",
        "Empik",
        "DynastyScans",
        "Batoto",
        "ChainedSoldier",
        "Hitomi"
      )
    )
