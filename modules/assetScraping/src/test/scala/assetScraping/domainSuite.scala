package assetScraping

import java.net.URI

import cats.syntax.all.*
import library.domain.AssetId

import configs.domain.*

class domainSuite extends munit.FunSuite:
  List(
    (
      "Mismatched uri",
      Site.Mangakakalot,
      "https://mangadex.org/title/296cbc31-af1a-4b5b-a34b-fee2b4cad542/-oshi-no-ko"
    ),
    (
      "Mismatched uri",
      Site.Mangadex,
      "https://chapmanganato.to/manga-ku987903"
    ),
    (
      "Missing manga id",
      Site.Mangadex,
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
        Site.Mangadex,
        IsConfigEnabled(true),
        AssetId(42)
      )
    configs match
      case Left(error) =>
        assert(false, s"At least one config creation failed due to $error")
      case Right(configs) =>
        assertEquals(configs.map(_.uri.value.toString).toSet.size, 1)
