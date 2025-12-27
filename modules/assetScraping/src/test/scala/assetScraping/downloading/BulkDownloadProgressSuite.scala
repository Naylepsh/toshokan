package assetScraping.downloading

import assetScraping.downloading.domain.BulkDownloadProgress
import library.domain.{AssetId, EntryId, EntryNo}
import munit.CatsEffectSuite

class BulkDownloadProgressSuite extends CatsEffectSuite:

  test("isComplete returns true when all entries processed"):
    val progress = BulkDownloadProgress(
      assetId = AssetId(1),
      totalEntries = 3,
      completedEntries = 2,
      failedEntries = List(EntryNo("3")),
      currentEntry = None
    )
    assert(progress.isComplete)

  test("isComplete returns false when entries still pending"):
    val progress = BulkDownloadProgress(
      assetId = AssetId(1),
      totalEntries = 3,
      completedEntries = 1,
      failedEntries = List.empty,
      currentEntry = Some(EntryId(2))
    )
    assert(!progress.isComplete)

  test("successRate calculates correctly"):
    val progress = BulkDownloadProgress(
      assetId = AssetId(1),
      totalEntries = 4,
      completedEntries = 3,
      failedEntries = List(EntryNo("4")),
      currentEntry = None
    )
    assertEquals(progress.successRate, 0.75)

  test("successRate handles zero total entries"):
    val progress = BulkDownloadProgress(
      assetId = AssetId(1),
      totalEntries = 0,
      completedEntries = 0,
      failedEntries = List.empty,
      currentEntry = None
    )
    assertEquals(progress.successRate, 1.0)
