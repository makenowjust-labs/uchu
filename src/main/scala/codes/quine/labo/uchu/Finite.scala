package codes.quine.labo.uchu

/** Finite is a type-class for finite types.
  *
  * It is derived from Haskell's [[https://hackage.haskell.org/package/universe universe]] library.
  */
trait Finite[A] extends Universe[A] {

  /** A cardinality of the type. */
  def cardinality: BigInt
}

/** Finite utilities and instances. */
object Finite {

  /** Summons the instance. */
  @inline def apply[A](implicit A: Finite[A]): Finite[A] = A

  /** Builds an instance from a small list. */
  def of[A](xs: LazyList[A]): Finite[A] = new Finite[A] {
    def universe: LazyList[A] = xs
    def cardinality: BigInt = xs.size
  }

  /** Builds an instance from a lazy list and its size. */
  def of[A](xs: => LazyList[A], size: => BigInt): Finite[A] = new Finite[A] {
    def universe: LazyList[A] = xs
    def cardinality: BigInt = size
  }

  /** An instance for [[Nothing]]. */
  implicit def nothing: Finite[Nothing] = Finite.of(LazyList.empty)

  /** An instance for [[Unit]]. */
  implicit def unit: Finite[Unit] = Finite.of(LazyList(()))

  /** An instance for [[Boolean]]. */
  implicit def boolean: Finite[Boolean] = Finite.of(Enum.boolean)

  /** An instance for [[Byte]]. */
  implicit def byte: Finite[Byte] =
    Finite.of(Enum.bigInt.takeWhile(n => n >= Byte.MinValue && n <= Byte.MaxValue).map(_.toByte), (2: BigInt).pow(8))

  /** An instance for [[Short]]. */
  implicit def short: Finite[Short] =
    Finite.of(
      Enum.bigInt.takeWhile(n => n >= Short.MinValue && n <= Short.MaxValue).map(_.toShort),
      (2: BigInt).pow(16)
    )

  /** An instance for [[Int]]. */
  implicit def int: Finite[Int] =
    Finite.of(Enum.bigInt.takeWhile(n => n >= Int.MinValue && n <= Int.MaxValue).map(_.toInt), (2: BigInt).pow(32))

  /** An instance for [[Long]]. */
  implicit def long: Finite[Long] =
    Finite.of(Enum.bigInt.takeWhile(n => n >= Long.MinValue && n <= Long.MaxValue).map(_.toLong), (2: BigInt).pow(64))

  /** An instance for [[Tuple2]]. */
  implicit def tuple2[A, B](implicit A: Finite[A], B: Finite[B]): Finite[(A, B)] =
    Finite.of(Enum.tuple2(A.universe, B.universe), A.cardinality * B.cardinality)

  /** An instance for [[Option]]. */
  implicit def option[A](implicit A: Finite[A]): Finite[Option[A]] =
    Finite.of(Enum.option(A.universe), 1 + A.cardinality)

  /** An instance for [[Either]]. */
  implicit def either[A, B](implicit A: Finite[A], B: Finite[B]): Finite[Either[A, B]] =
    Finite.of(Enum.either(A.universe, B.universe), A.cardinality + B.cardinality)

  /** An instance for [[Map]]. */
  implicit def map[A, B](implicit A: Finite[A], B: Finite[B]): Finite[Map[A, B]] =
    Finite.of(
      Enum.map(A.universe, A.cardinality, B.universe), {
        require(A.cardinality <= Int.MaxValue)
        (B.cardinality + 1).pow(A.cardinality.toInt)
      }
    )

  /** An instance for [[Set]].
    *
    * We can't derive an `Universe[Set[A]]` from `Universe[A]`
    * because the cardinality of `Set[A]` is strictly greater than `A`.
    */
  implicit def set[A](implicit A: Finite[A]): Finite[Set[A]] =
    Finite.of(
      Enum.set(A.universe, A.cardinality), {
        require(A.cardinality <= Int.MaxValue)
        (2: BigInt).pow(A.cardinality.toInt)
      }
    )

  /** An instance for [[Function2]]. */
  implicit def function2[A, B](implicit A: Finite[A], B: Finite[B]): Finite[A => B] =
    Finite.of(
      Enum.function2(A.universe, A.cardinality, B.universe), {
        require(A.cardinality <= Int.MaxValue)
        B.cardinality.pow(A.cardinality.toInt)
      }
    )

  /** An instance for [[PartialFunction]]. It is same as [[Finite.map]] internally. */
  implicit def partialFunction[A, B](implicit A: Finite[A], B: Finite[B]): Finite[PartialFunction[A, B]] =
    Finite.of(
      Enum.map(A.universe, A.cardinality, B.universe), {
        require(A.cardinality <= Int.MaxValue)
        (B.cardinality + 1).pow(A.cardinality.toInt)
      }
    )
}
