package assetScraping.scrapes

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobiex.*
import library.domain.AssetId

import domain.*

trait ScrapeRepository[F[_]]:
  def addOrUpdate(
      assetIds: NonEmptyList[AssetId],
      scrapeDate: PastScrapeCreatedAt
  ): F[Unit]

object ScrapeRepository:
  private object PastScrapes extends TableDefinition("past_scrapes"):
    val id        = Column[Long]("id")
    val assetId   = Column[AssetId]("asset_id")
    val createdAt = Column[PastScrapeCreatedAt]("created_at")

    val * = Columns((assetId, createdAt))

  def make[F[_]: MonadCancelThrow](
      xa: Transactor[F]
  ): ScrapeRepository[F] = new:
    override def addOrUpdate(
        assetIds: NonEmptyList[AssetId],
        scrapeDate: PastScrapeCreatedAt
    ): F[Unit] =
      val values = assetIds.map: assetId =>
        fr"(${assetId}, ${scrapeDate})"
      val valuesFrag = values.tail.foldLeft(values.head)(_ ++ fr"," ++ _)

      sql"""
      INSERT INTO ${PastScrapes}(${PastScrapes.assetId}, ${PastScrapes.createdAt})
      VALUES ${valuesFrag}
      ON CONFLICT ${PastScrapes.assetId} DO UPDATE SET
        ${PastScrapes.createdAt === scrapeDate}
      """.update.run.transact(xa).void
