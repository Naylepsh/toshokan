package core

import scala.deriving.*

object Tuples:
  // Taken from https://taig.medium.com/converting-between-tuples-and-case-classes-in-scala-3-7079ccedf4c0
  def to[A <: Product](value: A)(using
      mirror: Mirror.ProductOf[A]
  ): mirror.MirroredElemTypes = Tuple.fromProductTyped(value)

  def from[A](value: Product)(using
      mirror: Mirror.ProductOf[A],
      ev: value.type <:< mirror.MirroredElemTypes
  ): A = mirror.fromProduct(value)
