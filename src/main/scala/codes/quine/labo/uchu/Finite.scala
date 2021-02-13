package codes.quine.labo.uchu

import codes.quine.labo.uchu.Cardinality.Fin

/** Finite is a type-class for finite types.
  *
  * It is derived from Haskell's [[https://hackage.haskell.org/package/universe universe]] library.
  */
trait Finite[A] extends Universe[A] {

  /** A cardinality of the type. */
  def cardinality: Fin

  /** A finite size of the type. */
  def size: BigInt = cardinality.size

  override def toString: String = {
    val card =
      try cardinality
      catch { case _: ArithmeticException => "<error>" }
    s"Finite.of($enumerate, $card)"
  }
}

/** Finite utilities and instances. */
object Finite {

  /** Summons the instance. */
  @inline def apply[A](implicit A: Finite[A]): Finite[A] = A

  /** Builds an instance from a small list. */
  def of[A](xs: LazyList[A]): Finite[A] = of(xs, xs.size)

  /** Builds an instance from a lazy list and its size. */
  def of[A](xs: => LazyList[A], size: => BigInt): Finite[A] = of(xs, Fin(size))

  /** Builds an instance from a lazy list and its cardinality. */
  def of[A](xs: => LazyList[A], card: => Fin)(implicit dummy: DummyImplicit): Finite[A] = new Finite[A] {
    def enumerate: LazyList[A] = xs
    def cardinality: Fin = card
  }

  /** An instance for [[Nothing]]. */
  implicit def nothing: Finite[Nothing] = of(LazyList.empty)

  /** An instance for [[Unit]]. */
  implicit def unit: Finite[Unit] = of(LazyList(()))

  /** An instance for [[Boolean]]. */
  implicit def boolean: Finite[Boolean] = of(Enumerate.boolean)

  /** An instance for [[Byte]]. */
  implicit def byte: Finite[Byte] = of(Enumerate.byte, (2: BigInt).pow(8))

  /** An instance for [[Short]]. */
  implicit def short: Finite[Short] = of(Enumerate.short, (2: BigInt).pow(16))

  /** An instance for [[Int]]. */
  implicit def int: Finite[Int] = of(Enumerate.int, (2: BigInt).pow(32))

  /** An instance for [[Long]]. */
  implicit def long: Finite[Long] = of(Enumerate.long, (2: BigInt).pow(64))

  /** An instance for [[Tuple2]]. */
  implicit def tuple2[A, B](implicit A: Finite[A], B: Finite[B]): Finite[(A, B)] =
    of(Enumerate.tuple2(A.enumerate, B.enumerate), A.cardinality * B.cardinality)

  /** An instance for [[Option]]. */
  implicit def option[A](implicit A: Finite[A]): Finite[Option[A]] =
    of(Enumerate.option(A.enumerate), A.cardinality + 1)

  /** An instance for [[Either]]. */
  implicit def either[A, B](implicit A: Finite[A], B: Finite[B]): Finite[Either[A, B]] =
    of(Enumerate.either(A.enumerate, B.enumerate), A.cardinality + B.cardinality)

  /** An instance for [[Map]]. */
  implicit def map[A, B](implicit A: Finite[A], B: Finite[B]): Finite[Map[A, B]] =
    of(Enumerate.map(A.enumerate, A.size, B.enumerate), (B.cardinality + 1) ** A.cardinality)

  /** An instance for [[Set]].
    *
    * We can't derive an `Universe[Set[A]]` from `Universe[A]`
    * because the cardinality of `Set[A]` is strictly greater than `A`.
    */
  implicit def set[A](implicit A: Finite[A]): Finite[Set[A]] =
    of(Enumerate.set(A.enumerate, A.size), (2: BigInt).pow(A.cardinality.toInt))

  /** An instance for [[Function2]]. */
  implicit def function2[A, B](implicit A: Finite[A], B: Finite[B]): Finite[A => B] =
    of(Enumerate.function2(A.enumerate, A.size, B.enumerate), B.cardinality ** A.cardinality)

  /** An instance for [[PartialFunction]]. It is same as [[Finite.map]] internally. */
  implicit def partialFunction[A, B](implicit A: Finite[A], B: Finite[B]): Finite[PartialFunction[A, B]] =
    of(Enumerate.map(A.enumerate, A.size, B.enumerate), (B.cardinality + 1) ** A.cardinality)
}
