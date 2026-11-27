package entryMedia
package domain

type TagNamespace = TagNamespace.Type
object TagNamespace extends neotype.Subtype[String]

type TagName = TagName.Type
object TagName extends neotype.Subtype[String]

case class Tag(namespace: TagNamespace, name: TagName):
  /** The form accepted when entering tags */
  def show: String =
    if namespace == Tag.defaultNamespace then name else s"$namespace:$name"

  /** A search token that parses back into this tag */
  def asSearchToken: String =
    val value = if name.exists(_.isWhitespace) then s"\"$name\"" else name
    s"$namespace:$value"

object Tag:
  val defaultNamespace = TagNamespace("tag")

  // quotes are dropped so that search tokens always round-trip
  private def normalize(raw: String) =
    raw.replace("\"", "").trim.toLowerCase.replaceAll("\\s+", " ")

  def of(namespace: String, name: String): Option[Tag] =
    val (ns, n) = (normalize(namespace), normalize(name))
    Option.when(ns.nonEmpty && n.nonEmpty)(Tag(TagNamespace(ns), TagName(n)))

  /** `namespace:name`, or just `name` for the default namespace */
  def parse(raw: String): Option[Tag] =
    raw.indexOf(':') match
      case -1 => of(defaultNamespace, raw)
      case i  => of(raw.take(i), raw.drop(i + 1))

  /** Comma-separated tags */
  def parseList(input: String): List[Tag] =
    input.split(',').toList.flatMap(parse).distinct

case class SearchQuery(words: List[String], tags: List[Tag])

object SearchQuery:
  val empty = SearchQuery(Nil, Nil)

  // key:"quoted value" | "quoted phrase" | anything-else
  private val token = """([^\s:"]+):"([^"]*)"|"([^"]*)"|(\S+)""".r

  /** `key:value` tokens are tags, everything else is free text */
  def parse(input: String): SearchQuery =
    val parsed = token
      .findAllMatchIn(input)
      .foldLeft(empty): (acc, m) =>
        if m.group(1) != null then
          Tag
            .of(m.group(1), m.group(2))
            .fold(acc)(tag => acc.copy(tags = acc.tags :+ tag))
        else if m.group(3) != null then addWord(acc, m.group(3))
        else
          val raw = m.group(4)
          Option
            .when(raw.contains(':'))(Tag.parse(raw))
            .flatten
            .fold(addWord(acc, raw))(tag => acc.copy(tags = acc.tags :+ tag))
    parsed.copy(tags = parsed.tags.distinct)

  private def addWord(query: SearchQuery, word: String) =
    if word.isBlank then query
    else query.copy(words = query.words :+ word.trim)
