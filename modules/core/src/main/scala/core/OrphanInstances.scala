package core

import java.net.URI

import cats.kernel.Order
import cats.syntax.all.*
import doobie.util.{ Get, Put }
import doobie.implicits.*

given Order[URI] = Order[String].contramap(_.toString)
given Get[URI]   = Get[String].map(URI(_))
given Put[URI]   = Put[String].contramap(_.toString)
