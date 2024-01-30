package core

import java.net.URI

import cats.kernel.Order
import cats.syntax.all.*
import com.augustnagro.magnum.DbCodec

given Order[URI]   = Order[String].contramap(_.toString)
given DbCodec[URI] = DbCodec[String].biMap(URI(_), _.toString)
