package core

import java.net.URI
import java.time.LocalDate

import cats.kernel.Order
import cats.syntax.all.*
import doobie.util.{ Get, Put }
import io.circe.Decoder

given Order[URI]   = Order[String].contramap(_.toString)
given Get[URI]     = Get[String].map(URI(_))
given Put[URI]     = Put[String].contramap(_.toString)
given Decoder[URI] = Decoder.decodeString.emap(Uri.fromStringSafe)

given Get[LocalDate] = Get[String].map(LocalDate.parse(_))
given Put[LocalDate] = Put[String].contramap(_.toString)
