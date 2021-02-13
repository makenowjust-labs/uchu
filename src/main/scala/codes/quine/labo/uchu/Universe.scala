package codes.quine.labo.uchu

import Cardinality.Inf

/** Universe is a type-class for enumerable types.
  *
  * It is derived from Haskell's [[https://hackage.haskell.org/package/universe universe]] library.
  */
trait Universe[A] extends Serializable {

  /** Enumerates all possible values of the type. */
  def enumerate: LazyList[A]

  /** A cardinality of the type. */
  def cardinality: Cardinality

  override def toString: String = {
    val card = try cardinality catch { case _: ArithmeticException => "<error>" }
    s"Universe.of($enumerate, $card)"
  }
}

/** Universe utilities and instances. */
object Universe {

  /** Summons the instance. */
  @inline def apply[A](implicit A: Universe[A]): Universe[A] = A

  /** Builds an instance for an infinite type from a lazy list. */
  def of[A](xs: => LazyList[A]): Universe[A] = of(xs, Inf)

  /** Builds an instance from a lazy list and its cardinality. */
  def of[A](xs: => LazyList[A], card: => Cardinality): Universe[A] = new Universe[A] {
    def enumerate: LazyList[A] = xs
    def cardinality: Cardinality = card
  }

  /** All finite types are also recursive enumerable. */
  implicit def finite[A](implicit A: Finite[A]): Universe[A] = A

  /** An instance for [[BigInt]].
    *
    * Honestly, [[BigInt]] is finite because its available value is up to 2^Int.MaxValue^,
    * but it is considered as infinite for usual purpose.
    */
  implicit def bigInt: Universe[BigInt] = of(Enumerate.bigInt)

  /** An instance for [[Tuple2]]. */
  implicit def tuple2[A, B](implicit A: Universe[A], B: Universe[B]): Universe[(A, B)] =
    of(Enumerate.tuple2(A.enumerate, B.enumerate), A.cardinality * B.cardinality)

  /** An instance for [[List]]. */
  implicit def list[A](implicit A: Universe[A]): Universe[List[A]] =
    of(Enumerate.list(A.enumerate))

  /** An instance for [[Map]]. */
  implicit def map[A, B](implicit A: Finite[A], B: Universe[B]): Universe[Map[A, B]] =
    of(Enumerate.map(A.enumerate, A.size, B.enumerate), (B.cardinality + 1) ** A.cardinality)

  /** An instance for [[Option]]. */
  implicit def option[A](implicit A: Universe[A]): Universe[Option[A]] =
    of(Enumerate.option(A.enumerate), A.cardinality + 1)

  /** An instance for [[Either]]. */
  implicit def either[A, B](implicit A: Universe[A], B: Universe[B]): Universe[Either[A, B]] =
    of(Enumerate.either(A.enumerate, B.enumerate), A.cardinality + B.cardinality)

  /** An instance for [[Function1]]. */
  implicit def function2[A, B](implicit A: Finite[A], B: Universe[B]): Universe[A => B] =
    of(Enumerate.function2(A.enumerate, A.size, B.enumerate), B.cardinality ** A.cardinality)

  /** An instance for [[PartialFunction]]. It is same as [[Universe.map]] internally. */
  implicit def partialFunction[A, B](implicit A: Finite[A], B: Universe[B]): Universe[PartialFunction[A, B]] =
    of(Enumerate.map(A.enumerate, A.cardinality.size, B.enumerate), (B.cardinality + 1) ** A.cardinality)
}
