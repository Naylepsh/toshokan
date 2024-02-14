package core

import java.net.URI

import cats.kernel.Order
import cats.syntax.all.*
import doobie.util.{ Get, Put }
import doobie.implicits.*
import io.circe.Decoder
import org.apache.commons.validator.UrlValidator

private val urlValidator = new UrlValidator(Array("http", "https"))

given Order[URI] = Order[String].contramap(_.toString)
given Get[URI]   = Get[String].map(URI(_))
given Put[URI]   = Put[String].contramap(_.toString)
given Decoder[URI] = Decoder.decodeString.emap: str =>
  if urlValidator.isValid(str) then URI(str).asRight
  else Left(s"$str is not a valid URI")
