package codes.quine.labo.uchu.laws

import codes.quine.labo.uchu.Card._
import codes.quine.labo.uchu.Nat
import codes.quine.labo.uchu.Universe

/** UniverseLaws are laws Universe must obey. */
trait UniverseLaws[A] {

  /** The Universe instance for A. */
  implicit val universe: Universe[A]

  /** `indexOf identity`: `(get compose indexOf)(x)` equals to `Some(x)`. */
  def universeIndexOfIdentity(x: A): IsEq[Option[A]] =
    universe.get(universe.indexOf(x)) <-> Some(x)

  /** `get identity`: `(map(indexOf) compose get)(x)` equals to `Some(k)` if k is smaller than `card`. */
  def universeGetIdentity(k: Nat): IsEq[Option[Nat]] =
    universe.get(k).map(universe.indexOf) <-> (if (Small(k) < universe.card) Some(k) else None)

  /** `enumerate consistency`: `k`-th element of `enumerate` equals to `get(k)`. */
  def universeEnumerateConsistency(k: SmallNat): IsEq[Option[A]] =
    universe.enumerate.lift(k.toInt) <-> universe.get(k.toNat)
}

/** UniverseLaws utilities. */
object UniverseLaws {

  /** Returns an UniverseLaws instance. */
  def apply[A](implicit A: Universe[A]): UniverseLaws[A] =
    new UniverseLaws[A] { val universe: Universe[A] = A }
}
