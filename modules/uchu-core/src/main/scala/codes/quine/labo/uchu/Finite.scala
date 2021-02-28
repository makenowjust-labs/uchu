package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

/** Finite is a type-class for finite types.
  *
  * It is derived from Haskell's [[https://hackage.haskell.org/package/universe universe]] library.
  */
trait Finite[A] extends Universe[A] {

  /** A cardinality of the type. */
  override def card: Fin

  /** Converts this by the given transformations. */
  override def imap[B](f: A => B)(g: B => A): Finite[B] =
    Finite.of(enumerate.map(f), card, indexOf.contramap(g), get.map(f))

  override def toString: String = s"Finite.of($enumerate, $indexOf, $get, $card)"
}

/** Finite utilities and instances. */
object Finite {

  /** Summons the instance. */
  @inline def apply[A](implicit A: Finite[A]): Finite[A] = A

  /** Builds an instance from a small list. */
  def of[A](xs: LazyList[A], i: IndexOf[A], g: Get[A]): Finite[A] = of(xs, Nat(xs.size), i, g)

  /** Builds an instance from a lazy list and its size. */
  def of[A](xs: => LazyList[A], size: => Nat, i: IndexOf[A], g: Get[A]): Finite[A] = of(xs, Small(size), i, g)

  /** Builds an instance from a lazy list and its cardinality. */
  def of[A](xs: => LazyList[A], c: => Fin, i: IndexOf[A], g: Get[A])(implicit dummy: DummyImplicit): Finite[A] =
    new Finite[A] {
      def enumerate: LazyList[A] = xs
      def card: Fin = c
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
  implicit def byte: Finite[Byte] = of(Enumerate.byte, Nat.ByteSize, IndexOf.byte, Get.byte)

  /** An instance for [[Short]]. */
  implicit def short: Finite[Short] = of(Enumerate.short, Nat.ShortSize, IndexOf.short, Get.short)

  /** An instance for [[Int]]. */
  implicit def int: Finite[Int] = of(Enumerate.int, Nat.IntSize, IndexOf.int, Get.int)

  /** An instance for [[Long]]. */
  implicit def long: Finite[Long] = of(Enumerate.long, Nat.LongSize, IndexOf.long, Get.long)

  /** An instance for [[Char]]. */
  implicit def char: Finite[Char] = of(Enumerate.char, Nat.CharSize, IndexOf.char, Get.char)

  /** An instance for [[Tuple2]]. */
  implicit def tuple2[A, B](implicit A: Finite[A], B: Finite[B]): Finite[(A, B)] = of(
    Enumerate.tuple2(A.enumerate, B.enumerate),
    A.card * B.card,
    IndexOf.tuple2(A.indexOf, A.card, B.indexOf, B.card),
    Get.tuple2(A.get, A.card, B.get, B.card)
  )

  /** An instance for [[Option]]. */
  implicit def option[A](implicit A: Finite[A]): Finite[Option[A]] = of(
    Enumerate.option(A.enumerate),
    A.card + 1,
    IndexOf.option(A.indexOf),
    Get.option(A.get)
  )

  /** An instance for [[Either]]. */
  implicit def either[A, B](implicit A: Finite[A], B: Finite[B]): Finite[Either[A, B]] = of(
    Enumerate.either(A.enumerate, B.enumerate),
    A.card + B.card,
    IndexOf.either(A.indexOf, A.card, B.indexOf, B.card),
    Get.either(A.get, A.card, B.get, B.card)
  )

  /** An instance for [[Map]]. */
  implicit def map[A, B](implicit A: Finite[A], B: Finite[B]): Finite[Map[A, B]] = of(
    Enumerate.map(A.enumerate, A.card, B.enumerate),
    (B.card + 1) ** A.card,
    IndexOf.map(A.indexOf, A.card, B.indexOf, B.card),
    Get.map(A.get, A.card, B.get, B.card)
  )

  /** An instance for [[Set]].
    *
    * We can't derive an `Universe[Set[A]]` from `Universe[A]`
    * because the cardinality of `Set[A]` is strictly greater than `A`.
    */
  implicit def set[A](implicit A: Finite[A]): Finite[Set[A]] = of(
    Enumerate.set(A.enumerate, A.card),
    Two ** A.card,
    IndexOf.set(A.indexOf),
    Get.set(A.get, A.card)
  )

  /** An instance for [[Function1]]. */
  implicit def function1[A, B](implicit A: Finite[A], B: Finite[B]): Finite[A => B] = of(
    Enumerate.function1(A.indexOf, A.card, B.enumerate),
    B.card ** A.card,
    IndexOf.function1(A.enumerate, A.indexOf, A.card, B.indexOf, B.card),
    Get.function1(A.indexOf, A.card, B.get, B.card)
  )

  /** An instance for [[PartialFunction]]. It is same as [[Finite.map]] internally. */
  implicit def partialFunction[A, B](implicit A: Finite[A], B: Finite[B]): Finite[PartialFunction[A, B]] = of(
    Enumerate.map(A.enumerate, A.card, B.enumerate),
    (B.card + 1) ** A.card,
    IndexOf.partialFunction(A.enumerate, A.indexOf, A.card, B.indexOf, B.card),
    Get.partialFunction(A.get, A.card, B.get, B.card)
  )
}
