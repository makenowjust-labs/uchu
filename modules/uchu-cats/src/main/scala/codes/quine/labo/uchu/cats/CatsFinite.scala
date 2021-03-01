package codes.quine.labo.uchu.cats

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import cats.Eval
import cats.data.Cont
import cats.data.Ior
import cats.data.NonEmptyMap
import cats.data.NonEmptySet

import codes.quine.labo.uchu.Card._
import codes.quine.labo.uchu.Enumerate
import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.Get
import codes.quine.labo.uchu.IndexOf
import codes.quine.labo.uchu.UniverseOrdering

/** CatsFinite defines Finite instances for cats data types. */
trait CatsFinite {

  /** Finite instance for Eval. */
  implicit def uchuFiniteForCatsEval[A](implicit A: Finite[A]): Finite[Eval[A]] = A.imap(Eval.now)(_.value)

  /** Finite instance for Ior. */
  implicit def uchuFiniteForCatsDataIor[A, B](implicit A: Finite[A], B: Finite[B]): Finite[Ior[A, B]] = {
    def f(in: Either[A, Either[B, (A, B)]]): Ior[A, B] = in match {
      case Left(x)              => Ior.left(x)
      case Right(Left(y))       => Ior.right(y)
      case Right(Right((x, y))) => Ior.both(x, y)
    }

    def g(out: Ior[A, B]): Either[A, Either[B, (A, B)]] = out match {
      case Ior.Left(x)    => Left(x)
      case Ior.Right(y)   => Right(Left(y))
      case Ior.Both(x, y) => Right(Right((x, y)))
    }

    Finite[Either[A, Either[B, (A, B)]]].imap(f)(g)
  }

  /** Finite instance for NonEmptySet. */
  implicit def uchuFiniteForCatsDataNonEmptySet[A](implicit A: Finite[A]): Finite[NonEmptySet[A]] = {
    // Creates an Ordering instance for SortedSet construction.
    implicit val orderingForA: Ordering[A] = new UniverseOrdering[A]
    Finite
      .of(
        Enumerate.nonEmptySet(A.enumerate, A.card),
        Two ** A.card - 1,
        IndexOf.nonEmptySet(A.indexOf),
        Get.nonEmptySet(A.get, A.card)
      )
      .imap(set => NonEmptySet.fromSetUnsafe(SortedSet.from(set)))(_.toSortedSet)
  }

  /** Finite instance for NonEmptyMap. */
  implicit def uchuFiniteForCatsDataNonEmptyMap[A, B](implicit
      A: Finite[A],
      B: Finite[B]
  ): Finite[NonEmptyMap[A, B]] = {
    // Creates an Ordering instance for SortedMap construction.
    implicit val orderingForA: Ordering[A] = new UniverseOrdering[A]
    Finite
      .of(
        Enumerate.nonEmptyMap(A.enumerate, A.card, B.enumerate),
        (B.card + 1) ** A.card - 1,
        IndexOf.nonEmptyMap(A.indexOf, A.card, B.indexOf, B.card),
        Get.nonEmptyMap(A.get, A.card, B.get, B.card)
      )
      .imap(map => NonEmptyMap.fromMapUnsafe(SortedMap.from(map)))(_.toSortedMap)
  }

  /** Finite instance for Cont. */
  implicit def uchuFiniteForCatsDataCont[A, B](implicit A: Finite[A], B: Finite[B]): Finite[Cont[A, B]] =
    Finite[(B => Eval[A]) => Eval[A]].imap(Cont(_))(_.run)
}

/** Finite instances for cats data types. */
object CatsFinite extends CatsFinite
