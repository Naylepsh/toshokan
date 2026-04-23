package authorMerging

import assetMapping.MalMangaMappingRepository
import assetScraping.configs.{
  AssetScrapingConfigRepository,
  AuthorScrapingConfigRepository
}
import cats.data.NonEmptyList
import cats.effect.IO
import db.migrations.applyMigrations
import db.transactors.inMemoryTransactor
import doobie.*
import doobie.implicits.*
import library.asset.AssetRepository
import library.asset.domain.*
import library.author.AuthorRepository
import library.author.domain.*
import munit.CatsEffectSuite

class AuthorMergeServiceSuite extends CatsEffectSuite:

  val withService = ResourceFunFixture(
    inMemoryTransactor[IO]
      .evalTap(applyMigrations)
      .map: xa =>
        val authorRepo = AuthorRepository.make
        val assetRepo  = AssetRepository.make
        val authorScrapingConfigRepo =
          AuthorScrapingConfigRepository.make
        val assetScrapingConfigRepo = AssetScrapingConfigRepository.make
        val malMangaMappingRepo     = MalMangaMappingRepository.make
        val service = AuthorMergeService[IO](
          authorRepo,
          assetRepo,
          authorScrapingConfigRepo,
          assetScrapingConfigRepo,
          malMangaMappingRepo,
          xa
        )
        (service, authorRepo, xa)
  )

  withService.test("basic merge: relinks assets and records alias"):
    (service, authorRepo, xa) =>
      for
        // Setup: two authors with distinct assets
        _ <-
          sql"""INSERT INTO authors (id, name) VALUES (1, 'Author A'), (2, 'Author B')""".update.run
            .transact(xa)
        _ <-
          sql"""INSERT INTO assets (id, title) VALUES (10, 'Asset X'), (20, 'Asset Y')""".update.run
            .transact(xa)
        _ <-
          sql"""INSERT INTO assets_authors (asset_id, author_id) VALUES (10, 1), (20, 2)""".update.run
            .transact(xa)
        // Merge author 2 into author 1
        _ <- service.mergeAuthors(NonEmptyList.of(AuthorId(2)), AuthorId(1))
        // Author 2 should be deleted
        author2 <- authorRepo.find(AuthorId(2)).transact(xa)
        _ = assertEquals(author2, None)
        // Author 1 should still exist
        author1 <- authorRepo.find(AuthorId(1)).transact(xa)
        _ = assert(author1.isDefined)
        // Asset Y should now be linked to author 1
        assets <- authorRepo.findAssetsByAuthor(AuthorId(1)).transact(xa)
        _ = assertEquals(
          assets.map(_.title).toSet,
          Set(AssetTitle("Asset X"), AssetTitle("Asset Y"))
        )
        // Alias should be recorded
        alias <- sql"SELECT alias_name FROM author_aliases WHERE author_id = 1"
          .query[String]
          .to[List]
          .transact(xa)
        _ = assertEquals(alias, List("Author B"))
      yield ()

  withService.test("duplicate asset merge: entries and configs transferred"):
    (service, _, xa) =>
      for
        // Setup: two authors, one shared asset (same title = same row), one unique to source
        _ <-
          sql"""INSERT INTO authors (id, name) VALUES (1, 'Author A'), (2, 'Author B')""".update.run
            .transact(xa)
        _ <-
          sql"""INSERT INTO assets (id, title) VALUES (10, 'Shared Asset'), (20, 'Source Only')""".update.run
            .transact(xa)
        _ <-
          sql"""INSERT INTO assets_authors (asset_id, author_id) VALUES (10, 1), (10, 2), (20, 2)""".update.run
            .transact(xa)
        _ <-
          sql"""INSERT INTO asset_entries (id, title, no, uri, date_uploaded, asset_id) VALUES (100, 'Ch1', '1', 'http://a/1', '2026-01-01', 10)""".update.run
            .transact(xa)
        _ <-
          sql"""INSERT INTO asset_entries (id, title, no, uri, date_uploaded, asset_id) VALUES (200, 'Ch2', '2', 'http://b/2', '2026-01-02', 20)""".update.run
            .transact(xa)
        _ <-
          sql"""INSERT INTO asset_scraping_configs (id, uri, site, is_enabled, asset_id) VALUES (1, 'http://scrape/a', 'Mangadex', 1, 10)""".update.run
            .transact(xa)
        _ <-
          sql"""INSERT INTO asset_scraping_configs (id, uri, site, is_enabled, asset_id) VALUES (2, 'http://scrape/b', 'Mangadex', 1, 20)""".update.run
            .transact(xa)
        // Merge author 2 into author 1
        _ <- service.mergeAuthors(NonEmptyList.of(AuthorId(2)), AuthorId(1))
        // Source-only asset should be relinked to author 1
        assets <-
          sql"SELECT asset_id FROM assets_authors WHERE author_id = 1 ORDER BY asset_id"
            .query[Long]
            .to[List]
            .transact(xa)
        _ = assert(
          assets.contains(20L),
          s"Asset 20 should be linked to author 1, got: $assets"
        )
        // Author 2 should be deleted
        author2 <- sql"SELECT id FROM authors WHERE id = 2"
          .query[Long]
          .option
          .transact(xa)
        _ = assertEquals(author2, None)
      yield ()

  withService.test("author scraping configs transferred, duplicates ignored"):
    (service, _, xa) =>
      for
        _ <-
          sql"""INSERT INTO authors (id, name) VALUES (1, 'Author A'), (2, 'Author B')""".update.run
            .transact(xa)
        _ <-
          sql"""INSERT INTO assets (id, title) VALUES (10, 'Asset X')""".update.run
            .transact(xa)
        _ <-
          sql"""INSERT INTO assets_authors (asset_id, author_id) VALUES (10, 1)""".update.run
            .transact(xa)
        // Author 1 has one config, author 2 has a different config
        _ <-
          sql"""INSERT INTO author_scraping_configs (id, uri, site, is_enabled, author_id) VALUES (1, 'http://site/a', 'Hitomi', 1, 1)""".update.run
            .transact(xa)
        _ <-
          sql"""INSERT INTO author_scraping_configs (id, uri, site, is_enabled, author_id) VALUES (3, 'http://site/b', 'Hitomi', 1, 2)""".update.run
            .transact(xa)
        _ <- service.mergeAuthors(NonEmptyList.of(AuthorId(2)), AuthorId(1))
        // Both configs should now belong to author 1
        configs <-
          sql"SELECT uri FROM author_scraping_configs WHERE author_id = 1 ORDER BY uri"
            .query[String]
            .to[List]
            .transact(xa)
        _ = assertEquals(configs, List("http://site/a", "http://site/b"))
        // No configs for author 2
        sourceConfigs <-
          sql"SELECT id FROM author_scraping_configs WHERE author_id = 2"
            .query[Long]
            .to[List]
            .transact(xa)
        _ = assertEquals(sourceConfigs, List.empty)
      yield ()

  withService.test(
    "alias resolution: findOrAdd returns target author for old name"
  ): (service, authorRepo, xa) =>
    for
      _ <-
        sql"""INSERT INTO authors (id, name) VALUES (1, 'Author A'), (2, 'Author B')""".update.run
          .transact(xa)
      _ <-
        sql"""INSERT INTO assets (id, title) VALUES (10, 'Asset X')""".update.run
          .transact(xa)
      _ <-
        sql"""INSERT INTO assets_authors (asset_id, author_id) VALUES (10, 1)""".update.run
          .transact(xa)
      _ <- service.mergeAuthors(NonEmptyList.of(AuthorId(2)), AuthorId(1))
      // findOrAdd with old name should return the target author
      result <- authorRepo.findOrAdd(Set(AuthorName("Author B"))).transact(xa)
      _ = assertEquals(result.size, 1)
      _ = assertEquals(result.head.id, AuthorId(1))
      _ = assertEquals(result.head.name, AuthorName("Author A"))
    yield ()
