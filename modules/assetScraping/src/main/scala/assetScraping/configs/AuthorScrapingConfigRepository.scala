package assetScraping.configs

import assetScraping.configs.domain.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import core.Tuples
import core.given
import db.fragments.insertIntoReturning
import doobie.*
import doobie.implicits.*
import library.author.domain.AuthorId
import neotype.interop.doobie.given

trait AuthorScrapingConfigRepository:
  def add(
      scrapingConfig: NewAuthorScrapingConfig
  ): ConnectionIO[Either[AddAuthorScrapingConfig, ExistingAuthorScrapingConfig]]
  def findAllEnabled: ConnectionIO[List[ExistingAuthorScrapingConfig]]
  def findByAuthorId(
      authorId: AuthorId
  ): ConnectionIO[List[ExistingAuthorScrapingConfig]]
  def findById(
      id: AuthorScrapingConfigId
  ): ConnectionIO[Option[ExistingAuthorScrapingConfig]]
  def delete(id: AuthorScrapingConfigId): ConnectionIO[Boolean]
  def updateEnabled(
      id: AuthorScrapingConfigId,
      enabled: IsConfigEnabled
  ): ConnectionIO[Boolean]
  def transferConfigs(
      sourceIds: NonEmptyList[AuthorId],
      targetId: AuthorId
  ): ConnectionIO[Unit]

object AuthorScrapingConfigRepository:
  val make: AuthorScrapingConfigRepository = new:
    def add(
        scrapingConfig: NewAuthorScrapingConfig
    ): ConnectionIO[
      Either[AddAuthorScrapingConfig, ExistingAuthorScrapingConfig]
    ] =
      exists(scrapingConfig.uri).flatMap:
        case true =>
          AddAuthorScrapingConfig.ConfigAlreadyExists.asLeft.pure[ConnectionIO]
        case false => addWithoutChecking(scrapingConfig).map(_.asRight)

    def findAllEnabled: ConnectionIO[List[ExistingAuthorScrapingConfig]] =
      sql"""
      SELECT ${AuthorScrapingConfigs.*}
      FROM ${AuthorScrapingConfigs}
      WHERE ${AuthorScrapingConfigs.isEnabled === IsConfigEnabled(true)}
      """
        .queryOf(AuthorScrapingConfigs.*)
        .to[List]
        .map(_.map(Tuples.from[ExistingAuthorScrapingConfig](_)))

    def findByAuthorId(
        authorId: AuthorId
    ): ConnectionIO[List[ExistingAuthorScrapingConfig]] =
      sql"""
      SELECT ${AuthorScrapingConfigs.*}
      FROM ${AuthorScrapingConfigs}
      WHERE ${AuthorScrapingConfigs.authorId == authorId}
      """
        .queryOf(AuthorScrapingConfigs.*)
        .to[List]
        .map(_.map(Tuples.from[ExistingAuthorScrapingConfig](_)))

    def findById(
        id: AuthorScrapingConfigId
    ): ConnectionIO[Option[ExistingAuthorScrapingConfig]] =
      sql"""
      SELECT ${AuthorScrapingConfigs.*}
      FROM ${AuthorScrapingConfigs}
      WHERE ${AuthorScrapingConfigs.id == id}
      """
        .queryOf(AuthorScrapingConfigs.*)
        .option
        .map(_.map(Tuples.from[ExistingAuthorScrapingConfig](_)))

    def delete(id: AuthorScrapingConfigId): ConnectionIO[Boolean] =
      sql"""
      DELETE FROM ${AuthorScrapingConfigs}
      WHERE ${AuthorScrapingConfigs.id == id}
      """.update.run.map(_ > 0)

    def updateEnabled(
        id: AuthorScrapingConfigId,
        enabled: IsConfigEnabled
    ): ConnectionIO[Boolean] =
      sql"""
      UPDATE ${AuthorScrapingConfigs}
      SET ${AuthorScrapingConfigs.isEnabled} = $enabled
      WHERE ${AuthorScrapingConfigs.id == id}
      """.update.run.map(_ > 0)

    def transferConfigs(
        sourceIds: NonEmptyList[AuthorId],
        targetId: AuthorId
    ): ConnectionIO[Unit] =
      val inSource = Fragments.in(AuthorScrapingConfigs.authorId, sourceIds)
      for
        _ <-
          (sql"""UPDATE ${AuthorScrapingConfigs}
            SET ${AuthorScrapingConfigs.authorId} = $targetId
            WHERE """ ++ inSource ++ sql"""
              AND ${AuthorScrapingConfigs.uri} NOT IN (
                SELECT ${AuthorScrapingConfigs.uri} FROM ${AuthorScrapingConfigs}
                WHERE ${AuthorScrapingConfigs.authorId} = $targetId
              )""").update.run
        _ <-
          (sql"DELETE FROM ${AuthorScrapingConfigs} WHERE " ++ inSource).update.run
      yield ()

    private def exists(uri: ScrapingConfigUri): ConnectionIO[Boolean] =
      sql"""
      SELECT 1
      FROM ${AuthorScrapingConfigs}
      WHERE ${AuthorScrapingConfigs.uri == uri}
      """.query[Int].option.map(_.isDefined)

    private def addWithoutChecking(scrapingConfig: NewAuthorScrapingConfig) =
      insertIntoReturning(
        AuthorScrapingConfigs,
        NonEmptyList.of(
          _.uri --> scrapingConfig.uri,
          _.isEnabled --> scrapingConfig.isEnabled,
          _.site --> scrapingConfig.site,
          _.authorId --> scrapingConfig.authorId
        ),
        _.*
      )
        .queryOf(AuthorScrapingConfigs.*)
        .unique
        .map(Tuples.from[ExistingAuthorScrapingConfig](_))

private object AuthorScrapingConfigs
    extends TableDefinition("author_scraping_configs"):
  val id        = Column[AuthorScrapingConfigId]("id")
  val uri       = Column[ScrapingConfigUri]("uri")
  val isEnabled = Column[IsConfigEnabled]("is_enabled")
  val site      = Column[AuthorSite]("site")
  val authorId  = Column[AuthorId]("author_id")

  val *           = Columns((id, uri, site, isEnabled, authorId))
  val allExceptId = Columns((uri, site, isEnabled, authorId))
