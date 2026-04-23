package library.asset

import java.time.LocalDate

import cats.effect.IO
import core.types.PositiveInt
import db.migrations.applyMigrations
import db.transactors.inMemoryTransactor
import doobie.*
import doobie.implicits.*
import library.asset.domain.*
import munit.CatsEffectSuite

class AssetRepositorySuite extends CatsEffectSuite:

  val withRepo = ResourceFunFixture(
    inMemoryTransactor[IO]
      .evalTap(applyMigrations)
      .map: xa =>
        (AssetRepository.make, xa)
  )

  withRepo.test("findStale returns asset with no entries with None date"):
    (repo, xa) =>
      for
        _ <-
          sql"INSERT INTO assets (title) VALUES ('no-entries-asset')".update.run
            .transact(xa)
        results <- repo.findStale(PositiveInt(90)).transact(xa)
      yield
        assertEquals(results.length, 1)
        assertEquals(results.head._1.title, AssetTitle("no-entries-asset"))
        assertEquals(results.head._2, None)

  withRepo.test("findStale returns asset with old entry with Some(date)"):
    (repo, xa) =>
      val oldDate = LocalDate.now().minusDays(100).toString
      for
        _ <-
          sql"INSERT INTO assets (title) VALUES ('old-entry-asset')".update.run
            .transact(xa)
        assetId <-
          sql"SELECT id FROM assets WHERE title = 'old-entry-asset'"
            .query[Long]
            .unique
            .transact(xa)
        _ <-
          sql"INSERT INTO asset_entries (title, no, uri, date_uploaded, asset_id) VALUES ('ch1', '1', 'http://example.com/1', ${oldDate}, $assetId)".update.run
            .transact(xa)
        results <- repo.findStale(PositiveInt(90)).transact(xa)
      yield
        assertEquals(results.length, 1)
        assertEquals(
          results.head._2,
          Some(DateUploaded(LocalDate.parse(oldDate)))
        )

  withRepo.test("findStale does not return asset with recent entry"):
    (repo, xa) =>
      val recentDate = LocalDate.now().toString
      for
        _ <-
          sql"INSERT INTO assets (title) VALUES ('recent-entry-asset')".update.run
            .transact(xa)
        assetId <-
          sql"SELECT id FROM assets WHERE title = 'recent-entry-asset'"
            .query[Long]
            .unique
            .transact(xa)
        _ <-
          sql"INSERT INTO asset_entries (title, no, uri, date_uploaded, asset_id) VALUES ('ch1', '1', 'http://example.com/2', ${recentDate}, $assetId)".update.run
            .transact(xa)
        results <- repo.findStale(PositiveInt(90)).transact(xa)
      yield assertEquals(results.length, 0)
