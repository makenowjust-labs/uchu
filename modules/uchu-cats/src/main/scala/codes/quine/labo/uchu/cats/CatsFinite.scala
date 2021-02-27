package codes.quine.labo.uchu.cats

import cats.Eval
import cats.data.Ior

import codes.quine.labo.uchu.Finite

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
}

/** Finite instances for cats data types. */
object CatsFinite extends CatsFinite
