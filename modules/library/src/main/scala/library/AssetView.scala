package library

import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.EntityDecoder
import library.domain.ExistingAsset
import library.domain.ExistingAssetEntry

trait AssetView[F[_], A](using EntityDecoder[F, A]):
  val mediaType: MediaType
  def render(assetsViewEntries: List[(ExistingAsset, List[ExistingAssetEntry])])
      : A

object AssetView:
  def makeHtmlView[F[_]](using EntityDecoder[F, String]): AssetView[F, String] =
    new:
      val mediaType: MediaType = MediaType.text.html

      def render(assetsViewEntries: List[(
          ExistingAsset,
          List[ExistingAssetEntry]
      )]): String =
        "<div>Hello</div>"
