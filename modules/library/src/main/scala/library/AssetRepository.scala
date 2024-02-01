package library

import java.util.UUID
import javax.sql.DataSource

import scala.reflect.ClassTag

import cats.Applicative
import cats.syntax.all.*
import cats.effect.MonadCancelThrow
import library.domain.*
import io.github.arainko.ducktape.*
import doobie.*
import doobie.implicits.*
import doobie.syntax.SqlInterpolator.SingleFragment

trait AssetRepository[F[_], AssetId, AssetEntryId]:
  def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset[AssetId]]]
  def addEntries(
      assetId: AssetId,
      entry: NewAssetEntry
  ): F[Either[AddEntryError, ExistingAssetEntry[AssetEntryId]]]

object AssetRepository:
  case class Column[A: Read: Put](rawName: String):
    val name = Fragment.const0(f"${rawName}")
    val sql = name

    def read = summon[Read[A]]

  object Column:
    given [A]: Conversion[Column[A], SingleFragment[A]] =
      c => SingleFragment(fr"${c.name}")
    given [A]: Conversion[Column[A], Fragment] = c => fr"${c.name}"

  case class Columns[+A](sql: Fragment)
  object Columns:
    given Conversion[Columns[?], Fragment]          = _.sql
    given Conversion[Columns[?], SingleFragment[?]] = _.sql

    private def concatFrags(frags: Iterable[Fragment]): Fragment =
      frags
        .headOption
        .map: head =>
          frags.tail.foldLeft(head)(_ ++ fr"," ++ _)
        .getOrElse(fr0"")

    def apply[A1, A2](t: (Column[A1], Column[A2])): Columns[(A1, A2)] =
      import Column.*
      val frags = t.productIterator.map(_.asInstanceOf[Column[?]].sql)
      new Columns(concatFrags(Iterable.from(frags)))

  class TableDefinition(private val rawName: String):
    val name = Fragment.const0(rawName)
  object TableDefinition:
    given Conversion[TableDefinition, Fragment] = table => fr"${table.name}"
    given Conversion[TableDefinition, SingleFragment[Nothing]] =
      table => fr"${table.name}"

  extension (fragment: Fragment)
    inline def queryOf[A](col: Column[A])         = fragment.query[A](col.read)
    inline def queryOf[A: Read](cols: Columns[A]) = fragment.query[A]

  object Assets extends TableDefinition("assets"):
    val id    = Column[Long]("id")
    val title = Column[AssetTitle]("title")

    val * = Columns((id, title))

  def testSelectColumn[F[_]: MonadCancelThrow](xa: Transactor[F]) =
    val st = sql"SELECT ${Assets.id} FROM ${Assets}".queryOf(Assets.id)
    println("#" * 10)
    println(st.sql)
    st.to[List].transact(xa)

  def testSelectAllColumns[F[_]: MonadCancelThrow](xa: Transactor[F]) =
    val st = sql"SELECT ${Assets.*} FROM ${Assets}".queryOf(Assets.*)
    println("#" * 10)
    println(st.sql)
    st.to[List].transact(xa)

  def make[
      F[_]: MonadCancelThrow,
      AssetId: ClassTag,
      AssetEntryId: ClassTag
  ](xa: Transactor[F]): AssetRepository[F, AssetId, AssetEntryId] = new:
    def add(asset: NewAsset): F[Either[AddAssetError, ExistingAsset[AssetId]]] =
      val result = sql"SELECT ${Assets.id} FROM table".queryOf(
        Assets.id
      ).unique.transact(xa)

      // val result2 =
      //   sql"SELECT ${Assets.*} FROM table".queryOf(Assets.*).unique.transact(xa)
      //
      ???
    def addEntries(
        assetId: AssetId,
        entry: NewAssetEntry
    ): F[Either[AddEntryError, ExistingAssetEntry[AssetEntryId]]] = ???
