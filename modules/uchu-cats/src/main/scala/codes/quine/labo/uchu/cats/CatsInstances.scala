package codes.quine.labo.uchu.cats

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.kernel.LowerBoundedEnumerable
import cats.kernel.Order

import codes.quine.labo.uchu.Card
import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.Get
import codes.quine.labo.uchu.IndexOf
import codes.quine.labo.uchu.Nat
import codes.quine.labo.uchu.Universe
import codes.quine.labo.uchu.any2stringadd // Hack!

/** CatsInstances defines cats type-classes instances for uchu types. */
trait CatsInstances {
  // There is no InvariantMonoidal and MonoidK instances for Universe and Finite,
  // because their instance for Tuple2/Either is not associative.

  /** Cats type-classes instances for Universe. */
  implicit val catsInstancesForUchuUniverse: Invariant[Universe] = new Invariant[Universe] {
    def imap[A, B](fa: Universe[A])(f: A => B)(g: B => A): Universe[B] = fa.imap(f)(g)
  }

  /** Cats type-classes instances for Finite. */
  implicit val catsInstancesForUchuFinite: Invariant[Finite] = new Invariant[Finite] {
    def imap[A, B](fa: Finite[A])(f: A => B)(g: B => A): Finite[B] = fa.imap(f)(g)
  }

  /** Cats type-classes instances for IndexOf. */
  implicit val catsInstancesForUchuIndexOf: Contravariant[IndexOf] = new Contravariant[IndexOf] {
    def contramap[A, B](fa: IndexOf[A])(f: B => A): IndexOf[B] = fa.contramap(f)
  }

  /** Cats type-classes instances for Get. */
  implicit val catsInstancesForUchuGet: Functor[Get] = new Functor[Get] {
    def map[A, B](fa: Get[A])(f: A => B): Get[B] = fa.map(f)
  }

  /** Cats type-classes instances for Card. */
  implicit val catsInstancesForUchuCard: Order[Card] =
    new Order[Card] {
      override def compare(x: Card, y: Card): Int = x.compare(y)
    }

  /** Cats type-classes instances for Nat. */
  implicit val catsInstancesForUchuNat: LowerBoundedEnumerable[Nat] with Order[Nat] =
    new LowerBoundedEnumerable[Nat] with Order[Nat] {
      override def order: Order[Nat] = this
      override def partialPrevious(a: Nat): Option[Nat] = if (a == Nat.Zero) None else Some(a - 1)
      override def minBound: Nat = Nat.Zero
      override def next(a: Nat): Nat = a + 1
      override def compare(x: Nat, y: Nat): Int = x.compare(y)
    }
}

/** Cats type-classes instances for uchu types. */
object CatsInstances extends CatsInstances
