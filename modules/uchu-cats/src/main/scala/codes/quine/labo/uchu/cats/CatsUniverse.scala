package codes.quine.labo.uchu.cats

import cats.Eval
import cats.data.Chain
import cats.data.Ior
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptyVector

import codes.quine.labo.uchu.Universe

/** CatsUniverse defines Universe instances for cats data types. */
trait CatsUniverse {

  /** Universe instance for Eval. */
  implicit def uchuUniverseForCatsEval[A](implicit A: Universe[A]): Universe[Eval[A]] = A.imap(Eval.now)(_.value)

  /** Universe instance for Chain. */
  implicit def uchuUniverseForCatsDataChain[A](implicit A: Universe[A]): Universe[Chain[A]] =
    Universe[Seq[A]].imap(Chain.fromSeq)(_.toVector)

  /** Universe instance for NonEmptyChain. */
  implicit def uchuUniverseForCatsDataNonEmptyChain[A](implicit A: Universe[A]): Universe[NonEmptyChain[A]] =
    Universe[(A, Chain[A])].imap { case (x, xs) => NonEmptyChain.fromChainPrepend(x, xs) }(_.uncons)

  /** Universe instance for NonEmptySeq. */
  implicit def uchuUniverseForCatsDataNonEmptySeq[A](implicit A: Universe[A]): Universe[NonEmptySeq[A]] =
    Universe[(A, Seq[A])].imap { case (x, xs) => NonEmptySeq(x, xs) }(xs => (xs.head, xs.tail))

  /** Universe instance for NonEmptyList. */
  implicit def uchuUniverseForCatsDataNonEmptyList[A](implicit A: Universe[A]): Universe[NonEmptyList[A]] =
    Universe[(A, List[A])].imap { case (x, xs) => NonEmptyList(x, xs) }(xs => (xs.head, xs.tail))

  /** Universe instance for NonEmptyVector. */
  implicit def uchuUniverseForCatsDataNonEmptyVector[A](implicit A: Universe[A]): Universe[NonEmptyVector[A]] =
    Universe[(A, Vector[A])].imap { case (x, xs) => NonEmptyVector(x, xs) }(xs => (xs.head, xs.tail))

  /** Universe instance for Ior. */
  implicit def uchuUniverseForCatsDataIor[A, B](implicit A: Universe[A], B: Universe[B]): Universe[Ior[A, B]] = {
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

    Universe[Either[A, Either[B, (A, B)]]].imap(f)(g)
  }
}

/** Universe instances for cats data types. */
object CatsUniverse extends CatsUniverse
