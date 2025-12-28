package library.domain

import java.net.URI
import java.time.LocalDate

class ExistingAssetEntrySuite extends munit.FunSuite:
  import ExistingAssetEntry.*
  import ExistingAssetEntrySuite.*

  test("isLatestEntry is false for non-numeric"):
    assertEquals(
      isLatestEntry(EntryNo("The last chapter"), entries),
      false
    )

  test("isLatestEntry is false for fractional"):
    assertEquals(
      isLatestEntry(EntryNo("42.5"), entries),
      false
    )

  test("isLatestEntry is false for middle entry"):
    assertEquals(
      isLatestEntry(EntryNo("2"), entries),
      false
    )

  test("isLatestEntry is false when there are no entries to compare to"):
    assertEquals(
      isLatestEntry(EntryNo("2"), List.empty),
      false
    )

  test("isLatestEntry is true when the entry exactly that of the latest"):
    assertEquals(
      isLatestEntry(EntryNo("3"), entries),
      true
    )

object ExistingAssetEntrySuite:
  val entries = List(
    ExistingAssetEntry(
      EntryId(1),
      EntryTitle("The End of the Adventure"),
      EntryNo("1"),
      EntryUri(new URI("http://localhost:8080/foo/1")),
      DateUploaded(LocalDate.now()),
      AssetId(1)
    ),
    ExistingAssetEntry(
      EntryId(1),
      EntryTitle("The Priest's Lie"),
      EntryNo("2"),
      EntryUri(new URI("http://localhost:8080/foo/2")),
      DateUploaded(LocalDate.now()),
      AssetId(1)
    ),
    ExistingAssetEntry(
      EntryId(1),
      EntryTitle("Blue Moonweed"),
      EntryNo("3"),
      EntryUri(new URI("http://localhost:8080/foo/2")),
      DateUploaded(LocalDate.now()),
      AssetId(1)
    )
  )
