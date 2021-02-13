package codes.quine.labo.uchu

/** Universe is a type-class for enumerable types.
  *
  * It is derived from Haskell's [[https://hackage.haskell.org/package/universe universe]] library.
  */
trait Universe[A] extends Serializable {

  /** Enumerates all possible values of the type. */
  def universe: LazyList[A]
}

/** Universe utilities and instances. */
object Universe {

  /** Summons the instance. */
  @inline def apply[A](implicit A: Universe[A]): Universe[A] = A

  /** Builds an instance from a lazy list. */
  def of[A](xs: => LazyList[A]): Universe[A] = new Universe[A] {
    def universe: LazyList[A] = xs
  }

  /** All finite types are also recursive enumerable. */
  implicit def finite[A](implicit A: Finite[A]): Universe[A] = A

  /** An instance for [[BigInt]].
    *
    * Honestly, [[BigInt]] is finite because its available value is up to 2^Int.MaxValue^,
    * but it is considered as infinite for usual purpose.
    */
  implicit def bigInt: Universe[BigInt] = Universe.of(Enum.bigInt)

  /** An instance for [[Tuple2]]. */
  implicit def tuple2[A, B](implicit A: Universe[A], B: Universe[B]): Universe[(A, B)] =
    Universe.of(Enum.tuple2(A.universe, B.universe))

  /** An instance for [[List]]. */
  implicit def list[A](implicit A: Universe[A]): Universe[List[A]] =
    Universe.of(Enum.list(A.universe))

  /** An instance for [[Map]]. */
  implicit def map[A, B](implicit A: Finite[A], B: Universe[B]): Universe[Map[A, B]] =
    Universe.of(Enum.map(A.universe, A.cardinality, B.universe))

  /** An instance for [[Option]]. */
  implicit def option[A](implicit A: Universe[A]): Universe[Option[A]] =
    Universe.of(Enum.option(A.universe))

  /** An instance for [[Either]]. */
  implicit def either[A, B](implicit A: Universe[A], B: Universe[B]): Universe[Either[A, B]] =
    Universe.of(Enum.either(A.universe, B.universe))

  /** An instance for [[Function1]]. */
  implicit def function2[A, B](implicit A: Finite[A], B: Universe[B]): Universe[A => B] =
    Universe.of(Enum.function2(A.universe, A.cardinality, B.universe))

  /** An instance for [[PartialFunction]]. It is same as [[Universe.map]] internally. */
  implicit def partialFunction[A, B](implicit A: Finite[A], B: Universe[B]): Universe[PartialFunction[A, B]] =
    Universe.of(Enum.map(A.universe, A.cardinality, B.universe))
}
