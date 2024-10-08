package core

import cats.{Eq, Order, Show}
import doobie.{Put, Read}
import io.circe.{Decoder, Encoder}
import monocle.Iso

// this can be used for simple newtypes without pre-defined typeclass derivations
abstract class Newt[A]:
  /** Shamelessly copy-pasted from:
    * https://github.com/gvolpe/trading/blob/main/modules/domain/shared/src/main/scala/trading/Newtype.scala
    */
  opaque type Type = A
  inline def apply(a: A): Type            = a
  extension (t: Type) inline def value: A = t

  protected inline final def derive[F[_]](using ev: F[A]): F[Type] = ev

abstract class Newtype[A](using
    eqv: Eq[A],
    shw: Show[A],
    ord: Order[A],
    enc: Encoder[A],
    dec: Decoder[A],
    read: Read[A],
    readOpt: Read[Option[A]],
    put: Put[A]
):
  /** Shamelessly copy-pasted from:
    * https://github.com/gvolpe/trading/blob/main/modules/domain/shared/src/main/scala/trading/Newtype.scala
    */
  opaque type Type = A

  inline def apply(a: A): Type = a

  protected inline final def derive[F[_]](using ev: F[A]): F[Type] = ev

  extension (t: Type) inline def value: A = t

  given Wrapper[A, Type] with
    def iso: Iso[A, Type] =
      Iso[A, Type](apply(_))(_.value)

  given Eq[Type]           = eqv
  given Order[Type]        = ord
  given Show[Type]         = shw
  given Encoder[Type]      = enc
  given Decoder[Type]      = dec
  given Read[Type]         = read
  given Read[Option[Type]] = readOpt
  given Put[Type]          = put

  given Conversion[Type, A] = identity
