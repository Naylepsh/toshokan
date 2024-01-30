package core

import monocle.Iso

/**
 * The Wrapper typeclass defines an isomorphism that allows us to
 * convert back and forth from the underlying wrapped type.
 * -- G.Volpe from FEDA book
 */

trait Wrapper[A, B]:
  def iso: Iso[A, B]

object Wrapper:
  def apply[A, B](using ev: Wrapper[A, B]): Wrapper[A, B] = ev
