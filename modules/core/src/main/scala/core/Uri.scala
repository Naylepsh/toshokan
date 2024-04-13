package core

import java.net.URI

import cats.syntax.all.*
import org.apache.commons.validator.routines.UrlValidator

object Uri:
  private val urlValidator = new UrlValidator(Array("http", "https"))

  def fromStringSafe(str: String): Either[String, URI] =
    if urlValidator.isValid(str) then URI(str).asRight
    else Left(s"$str is not a valid URI")
