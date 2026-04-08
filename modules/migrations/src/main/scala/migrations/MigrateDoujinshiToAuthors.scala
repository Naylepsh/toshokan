package migrations

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import db.transactors.makeSqliteTransactorResource

object MigrateDoujinshiToAuthors extends IOApp.Simple:

  case class OldAsset(id: Long, title: String, configUri: String, configSite: String, configEnabled: Boolean)
  case class OldEntry(id: Long, title: String, no: String, uri: String, dateUploaded: String, oldAssetId: Long)

  def run: IO[Unit] =
    val dbUrl = sys.env("DATABASE_URL")
    val config = db.Config.forSqlite(db.Path(dbUrl))
    makeSqliteTransactorResource[IO](config).use: xa =>
      migrate(xa)

  private def migrate(xa: Transactor[IO]): IO[Unit] =
    val program = for
      _ <- logFC("Starting doujinshi → authors migration")

      doujinshiCategoryId <- sql"SELECT id FROM categories WHERE name = 'doujinshi'"
        .query[Long].unique

      oldAssets <- sql"""
        SELECT a.id, a.title, asc2.uri, asc2.site, asc2.is_enabled
        FROM assets a
        JOIN asset_scraping_configs asc2 ON asc2.asset_id = a.id
        WHERE a.category_id = $doujinshiCategoryId
      """.query[OldAsset].to[List]
      _ <- logFC(s"Found ${oldAssets.size} doujinshi assets to migrate")
      oldAssetIds = oldAssets.map(_.id).toSet

      oldEntries <- sql"""
        SELECT ae.id, ae.title, ae.no, ae.uri, ae.date_uploaded, ae.asset_id
        FROM asset_entries ae
        WHERE ae.asset_id IN (SELECT id FROM assets WHERE category_id = $doujinshiCategoryId)
      """.query[OldEntry].to[List]
      _ <- logFC(s"Found ${oldEntries.size} entries to migrate")

      // Step 1: Create authors
      authorNames = oldAssets.map(a => stripArtistSuffix(a.title)).distinct
      _ <- authorNames.traverse_(name =>
        sql"INSERT OR IGNORE INTO authors (name) VALUES ($name)".update.run
      )
      authorIdByName <- sql"SELECT id, name FROM authors"
        .query[(Long, String)].to[List].map(_.map((id, name) => name -> id).toMap)
      _ <- logFC(s"Created/found ${authorNames.size} authors")

      // Step 2: Create author_scraping_configs
      _ <- oldAssets.traverse_(createAuthorConfig(_, authorIdByName))
      _ <- logFC("Created author scraping configs")

      // Step 3: Remove FK from old entries so cascade delete won't touch them
      // We do this by first creating all new assets, then re-parenting, then deleting old
      entriesByAssetTitle = oldEntries.groupBy(e => (e.oldAssetId, e.title))
      _ <- logFC(s"${entriesByAssetTitle.size} unique (author, title) groups → new assets")

      // Step 4: Create new assets and collect mapping
      _ <- entriesByAssetTitle.toList.traverse_ { case ((oldAssetId, entryTitle), entries) =>
        val authorName = oldAssets.find(_.id == oldAssetId).map(a => stripArtistSuffix(a.title)).get
        val authorId = authorIdByName(authorName)
        for
          // Create new asset — exclude old doujinshi pseudo-assets from the lookup
          newAssetId <- sql"""
            SELECT id FROM assets
            WHERE title = $entryTitle
              AND id NOT IN (SELECT asset_id FROM asset_scraping_configs)
          """.query[Long].option.flatMap:
            case Some(id) => FC.pure(id)
            case None =>
              sql"INSERT INTO assets (title, category_id) VALUES ($entryTitle, $doujinshiCategoryId)"
                .update.withUniqueGeneratedKeys[Long]("id")
          _ <- sql"INSERT OR IGNORE INTO assets_authors (asset_id, author_id) VALUES ($newAssetId, $authorId)"
            .update.run
          // Re-parent entries immediately
          _ <- entries.traverse_(entry =>
            sql"UPDATE asset_entries SET asset_id = $newAssetId WHERE id = ${entry.id}".update.run
          )
        yield ()
      }
      _ <- logFC("Created new assets, linked authors, re-parented entries")

      // Verify entries are re-parented before deleting old assets
      orphanCheck <- sql"""
        SELECT COUNT(*) FROM asset_entries
        WHERE asset_id IN (SELECT id FROM assets WHERE category_id = $doujinshiCategoryId
          AND id IN (SELECT asset_id FROM asset_scraping_configs))
      """.query[Int].unique
      _ <- logFC(s"Entries still pointing to old assets: $orphanCheck")
      _ <- FC.raiseError[Unit](new RuntimeException("Entries still attached to old assets!"))
        .whenA(orphanCheck > 0)

      // Step 5: Delete old doujinshi pseudo-assets
      deleted <- sql"""
        DELETE FROM assets
        WHERE category_id = $doujinshiCategoryId
          AND id IN (SELECT asset_id FROM asset_scraping_configs WHERE site = 'hitomi')
      """.update.run
      _ <- logFC(s"Deleted $deleted old doujinshi pseudo-assets")

      // Step 6: Delete doujinshi schedule (keep category)
      _ <- sql"DELETE FROM scraping_schedules WHERE category_id = $doujinshiCategoryId"
        .update.run
      _ <- logFC("Deleted doujinshi schedule")

      // Verify
      authorCount   <- sql"SELECT COUNT(*) FROM authors".query[Int].unique
      newAssetCount <- sql"SELECT COUNT(*) FROM assets WHERE category_id = $doujinshiCategoryId".query[Int].unique
      linkCount     <- sql"SELECT COUNT(*) FROM assets_authors".query[Int].unique
      entryCount    <- sql"SELECT COUNT(*) FROM asset_entries".query[Int].unique
      progressCount <- sql"SELECT COUNT(*) FROM entry_progress".query[Int].unique
      orphanedEntries <- sql"SELECT COUNT(*) FROM asset_entries ae WHERE NOT EXISTS (SELECT 1 FROM assets a WHERE a.id = ae.asset_id)".query[Int].unique
      _ <- logFC(s"Done. Authors: $authorCount, Doujinshi assets: $newAssetCount, Links: $linkCount, Entries: $entryCount, Progress: $progressCount, Orphaned: $orphanedEntries")
    yield ()

    program.transact(xa)

  private def createAuthorConfig(asset: OldAsset, authorIdByName: Map[String, Long]): ConnectionIO[Unit] =
    val authorId = authorIdByName(stripArtistSuffix(asset.title))
    sql"""INSERT OR IGNORE INTO author_scraping_configs (uri, site, is_enabled, author_id)
          VALUES (${asset.configUri}, ${asset.configSite}, ${asset.configEnabled}, $authorId)
    """.update.run.void

  private def stripArtistSuffix(title: String): String =
    title match
      case s if s.endsWith(" (artist)") => s.dropRight(9)
      case s if s.endsWith(" (artst)")  => s.dropRight(8)
      case s                            => s

  private def logFC(msg: String): ConnectionIO[Unit] =
    FC.delay(scribe.info(msg))
