package codes.quine.labo.uchu.laws

import codes.quine.labo.uchu.Card._
import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.Nat
import codes.quine.labo.uchu.Universe

/** FiniteLaws are laws Finite must obey. */
trait FiniteLaws[A] extends UniverseLaws[A] {

  /** The Finite instance for A. */
  implicit val finite: Finite[A]
  override implicit lazy val universe: Universe[A] = finite

  /** "max index" law: `get(k)` is defined iff. `k` is smaller than `card`. */
  def finiteMaxIndex(k: Nat): IsEq[Boolean] =
    (Small(k) < universe.card) <-> universe.get(k).isDefined
}

/** FiniteLaws utilities. */
object FiniteLaws {

  /** Returns a FiniteLaws instance. */
  def apply[A](implicit A: Finite[A]): FiniteLaws[A] =
    new FiniteLaws[A] { implicit val finite: Finite[A] = A }
}
