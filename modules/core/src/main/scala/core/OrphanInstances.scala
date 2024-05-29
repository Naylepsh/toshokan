package core

import java.net.URI
import java.time.{DayOfWeek, LocalDate}

import scala.util.Try

import cats.{Show, Order}
import cats.syntax.all.*
import doobie.util.{Get, Put}
import io.circe.{Decoder, Encoder}

given Order[URI]   = Order[String].contramap(_.toString)
given Get[URI]     = Get[String].map(URI(_))
given Put[URI]     = Put[String].contramap(_.toString)
given Decoder[URI] = Decoder.decodeString.emap(Uri.fromStringSafe)
given Show[URI] with
  override def show(t: URI): String = t.toString

given Get[LocalDate] = Get[String].map(LocalDate.parse(_))
given Put[LocalDate] = Put[String].contramap(_.toString)

given Order[DayOfWeek]   = Order[Int].contramap(_.ordinal)
given Encoder[DayOfWeek] = Encoder.encodeInt.contramap(_.ordinal)
given Decoder[DayOfWeek] = Decoder.decodeInt.emapTry(i => Try(DayOfWeek.of(i)))
given Get[DayOfWeek]     = Get[Short].map(DayOfWeek.of)
given Put[DayOfWeek]     = Put[Short].contramap(_.getValue.toShort)
given Show[DayOfWeek] with
  override def show(t: DayOfWeek): String =
    val s = t.toString
    s.head.toString.capitalize + s.tail.toLowerCase
