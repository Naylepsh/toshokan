package core

import java.net.URI

import cats.kernel.Order
import cats.syntax.all.*

given Order[URI] = Order[String].contramap(_.toString)
