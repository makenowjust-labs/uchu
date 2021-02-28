package codes.quine.labo.uchu.shapeless

import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.Universe

/** semiauto is a collection of automatic derivations for uchu types. */
object semiauto {

  /** Derives a Universe instance for A. */
  implicit def universe[A](implicit A: MkUniverse[A]): Universe[A] = A

  /** Derives a Finite instance for A. */
  implicit def finite[A](implicit A: MkFinite[A]): Finite[A] = A
}
