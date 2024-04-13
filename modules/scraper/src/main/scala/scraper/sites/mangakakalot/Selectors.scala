package scraper.sites.mangakakalot

import java.net.URI

trait Selectors:
  val chapters: String
  val chapterName: String
  val chapterTimeUploaded: String

object Selectors:

  val mangakakalot = new Selectors:
    val chapters            = ".chapter-list .row"
    val chapterName         = "span:nth-of-type(1) a"
    val chapterTimeUploaded = "span:nth-of-type(3)"

  val manganato = new Selectors:
    val chapters            = ".row-content-chapter li"
    val chapterName         = "a"
    val chapterTimeUploaded = "span:nth-of-type(2)"

  private val mangakakalotUrlPattern = ".*mangakakalot.*".r
  private val manganatoUrlPattern    = ".*manganato.*".r
  def apply(url: URI): Option[Selectors] = url.toString match
    case mangakakalotUrlPattern() => Some(mangakakalot)
    case manganatoUrlPattern()    => Some(manganato)
    case _                        => None
