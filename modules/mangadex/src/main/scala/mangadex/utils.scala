package mangadex.utils

import java.net.URI

import cats.syntax.all.*

private val mangaIdFromUriPattern = "^https://mangadex.org/title/(.+)$".r

def extractMangaId(uri: URI): Either[String, String] =
  uri.toString match
    case mangaIdFromUriPattern(mangaId) => mangaId.asRight
    case _ => s"Could not extract manga id from uri:$uri".asLeft
