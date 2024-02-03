package core

import java.net.URI

import cats.kernel.Order
import cats.syntax.all.*
import doobie.util.{ Get, Put }
import doobie.implicits.*
import java.time.LocalDate
import java.time.format.DateTimeFormatter

given Order[URI] = Order[String].contramap(_.toString)
given Get[URI]   = Get[String].map(URI(_))
given Put[URI]   = Put[String].contramap(_.toString)

private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
given Get[LocalDate]  = Get[String].map(str => LocalDate.parse(str, formatter))
given Put[LocalDate]  = Put[String].contramap(_.toString)
