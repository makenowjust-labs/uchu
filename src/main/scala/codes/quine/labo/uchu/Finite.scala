package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

/** Finite is a type-class for finite types.
  *
  * It is derived from Haskell's [[https://hackage.haskell.org/package/universe universe]] library.
  */
trait Finite[A] extends Universe[A] {

  /** A cardinality of the type. */
  def cardinality: Fin

  override def toString: String = s"Finite.of($enumerate, $cardinality)"
}

/** Finite utilities and instances. */
object Finite {

  /** Summons the instance. */
  @inline def apply[A](implicit A: Finite[A]): Finite[A] = A

  /** Builds an instance from a small list. */
  def of[A](xs: LazyList[A], i: IndexOf[A], g: Get[A]): Finite[A] = of(xs, N(xs.size), i, g)

  /** Builds an instance from a lazy list and its size. */
  def of[A](xs: => LazyList[A], size: => N, i: IndexOf[A], g: Get[A]): Finite[A] = of(xs, Small(size), i, g)

  /** Builds an instance from a lazy list and its cardinality. */
  def of[A](xs: => LazyList[A], card: => Fin, i: IndexOf[A], g: Get[A])(implicit dummy: DummyImplicit): Finite[A] =
    new Finite[A] {
      def enumerate: LazyList[A] = xs
      def cardinality: Fin = card
      val indexOf: IndexOf[A] = i
      def get: Get[A] = g
    }

  /** An instance for [[Nothing]]. */
  implicit def nothing: Finite[Nothing] = of[Nothing](LazyList.empty, IndexOf.nothing, Get.nothing)

  /** An instance for [[Unit]]. */
  implicit def unit: Finite[Unit] = of(LazyList(()), IndexOf.unit, Get.unit)

  /** An instance for [[Boolean]]. */
  implicit def boolean: Finite[Boolean] = of(Enumerate.boolean, IndexOf.boolean, Get.boolean)

  /** An instance for [[Byte]]. */
  implicit def byte: Finite[Byte] = of(Enumerate.byte, N.Two ** 8, IndexOf.byte, Get.byte)

  /** An instance for [[Short]]. */
  implicit def short: Finite[Short] = of(Enumerate.short, N.Two ** 16, IndexOf.short, Get.short)

  /** An instance for [[Int]]. */
  implicit def int: Finite[Int] = of(Enumerate.int, N.Two ** 32, IndexOf.int, Get.int)

  /** An instance for [[Long]]. */
  implicit def long: Finite[Long] = of(Enumerate.long, N.Two ** 64, IndexOf.long, Get.long)

  /** An instance for [[Tuple2]]. */
  implicit def tuple2[A, B](implicit A: Finite[A], B: Finite[B]): Finite[(A, B)] = of(
    Enumerate.tuple2(A.enumerate, B.enumerate),
    A.cardinality * B.cardinality,
    IndexOf.tuple2(A.indexOf, A.cardinality, B.indexOf, B.cardinality),
    Get.tuple2(A.get, A.cardinality, B.get, B.cardinality)
  )

  /** An instance for [[Option]]. */
  implicit def option[A](implicit A: Finite[A]): Finite[Option[A]] = of(
    Enumerate.option(A.enumerate),
    A.cardinality + 1,
    IndexOf.option(A.indexOf),
    Get.option(A.get)
  )

  /** An instance for [[Either]]. */
  implicit def either[A, B](implicit A: Finite[A], B: Finite[B]): Finite[Either[A, B]] = of(
    Enumerate.either(A.enumerate, B.enumerate),
    A.cardinality + B.cardinality,
    IndexOf.either(A.indexOf, A.cardinality, B.indexOf, B.cardinality),
    Get.either(A.get, A.cardinality, B.get, B.cardinality)
  )

  /** An instance for [[Map]]. */
  implicit def map[A, B](implicit A: Finite[A], B: Finite[B]): Finite[Map[A, B]] = of(
    Enumerate.map(A.enumerate, A.cardinality, B.enumerate),
    (B.cardinality + 1) ** A.cardinality,
    IndexOf.map(A.indexOf, A.cardinality, B.indexOf, B.cardinality),
    Get.map(A.get, A.cardinality, B.get, B.cardinality)
  )

  /** An instance for [[Set]].
    *
    * We can't derive an `Universe[Set[A]]` from `Universe[A]`
    * because the cardinality of `Set[A]` is strictly greater than `A`.
    */
  implicit def set[A](implicit A: Finite[A]): Finite[Set[A]] = of(
    Enumerate.set(A.enumerate, A.cardinality),
    Two ** A.cardinality,
    IndexOf.set(A.indexOf),
    Get.set(A.get, A.cardinality)
  )

  /** An instance for [[Function1]]. */
  implicit def function1[A, B](implicit A: Finite[A], B: Finite[B]): Finite[A => B] = of(
    Enumerate.function1(A.enumerate, A.cardinality, B.enumerate),
    B.cardinality ** A.cardinality,
    IndexOf.function1(A.enumerate, A.cardinality, B.indexOf, B.cardinality),
    Get.function1(A.enumerate, A.cardinality, B.get, B.cardinality)
  )

  /** An instance for [[PartialFunction]]. It is same as [[Finite.map]] internally. */
  implicit def partialFunction[A, B](implicit A: Finite[A], B: Finite[B]): Finite[PartialFunction[A, B]] = of(
    Enumerate.map(A.enumerate, A.cardinality, B.enumerate),
    (B.cardinality + 1) ** A.cardinality,
    IndexOf.partialFunction(A.enumerate, A.indexOf, A.cardinality, B.indexOf, B.cardinality),
    Get.partialFunction(A.get, A.cardinality, B.get, B.cardinality)
  )
}
