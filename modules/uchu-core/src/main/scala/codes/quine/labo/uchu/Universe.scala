package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card.Inf

/** Universe is a type-class for countable types.
  *
  * It is derived from Haskell's [[https://hackage.haskell.org/package/universe universe]] library.
  */
trait Universe[A] extends Serializable {

  /** Enumerates all possible values of the type. */
  def enumerate: LazyList[A]

  /** Indexes a value. */
  def indexOf: IndexOf[A]

  /** Gets a value from an index. */
  def get: Get[A]

  /** A cardinality of the type. */
  def card: Card

  /** Converts this by the given transformations. */
  def imap[B](f: A => B)(g: B => A): Universe[B] =
    Universe.of(enumerate.map(f), card, indexOf.contramap(g), get.map(f))

  override def toString: String = s"Universe.of($enumerate, $indexOf, $get, $card)"
}

/** Universe utilities and instances. */
object Universe {

  /** Summons the instance. */
  @inline def apply[A](implicit A: Universe[A]): Universe[A] = A

  /** Builds an instance for an infinite type from a lazy list. */
  def of[A](xs: => LazyList[A], i: IndexOf[A], g: Get[A]): Universe[A] = of(xs, Inf, i, g)

  /** Builds an instance from a lazy list and its cardinality. */
  def of[A](xs: => LazyList[A], c: => Card, i: IndexOf[A], g: Get[A]): Universe[A] = new Universe[A] {
    def enumerate: LazyList[A] = xs
    def card: Card = c
    val indexOf: IndexOf[A] = i
    def get: Get[A] = g
  }

  /** All finite types are also recursive enumerable. */
  implicit def finite[A](implicit A: Finite[A]): Universe[A] = A

  /** An instance for [[BigInt]].
    *
    * Honestly, [[BigInt]] is finite because its available value is up to 2^Int.MaxValue^,
    * but it is considered as infinite for usual purpose.
    */
  implicit def bigInt: Universe[BigInt] = of(Enumerate.bigInt, IndexOf.bigInt, Get.bigInt)

  /** An instance for [[String]]. */
  implicit def string: Universe[String] = of(Enumerate.string, IndexOf.string, Get.string)

  /** An instance for [[Tuple2]]. */
  implicit def tuple2[A, B](implicit A: Universe[A], B: Universe[B]): Universe[(A, B)] = of(
    Enumerate.tuple2(A.enumerate, B.enumerate),
    A.card * B.card,
    IndexOf.tuple2(A.indexOf, A.card, B.indexOf, B.card),
    Get.tuple2(A.get, A.card, B.get, B.card)
  )

  /** An instance for [[Seq]]. */
  implicit def seq[A](implicit A: Universe[A]): Universe[Seq[A]] =
    of(Enumerate.seq(A.enumerate), IndexOf.seq(A.indexOf, A.card), Get.seq(A.get, A.card))

  /** An instance for [[List]]. */
  implicit def list[A](implicit A: Universe[A]): Universe[List[A]] = seq.imap(_.toList)(_.toSeq)

  /** An instance for [[Vector]]. */
  implicit def vector[A](implicit A: Universe[A]): Universe[Vector[A]] = seq.imap(_.toVector)(_.toSeq)

  /** An instance for [[Map]]. */
  implicit def map[A, B](implicit A: Finite[A], B: Universe[B]): Universe[Map[A, B]] = of(
    Enumerate.map(A.enumerate, A.card, B.enumerate),
    (B.card + 1) ** A.card,
    IndexOf.map(A.indexOf, A.card, B.indexOf, B.card),
    Get.map(A.get, A.card, B.get, B.card)
  )

  /** An instance for [[Option]]. */
  implicit def option[A](implicit A: Universe[A]): Universe[Option[A]] = of(
    Enumerate.option(A.enumerate),
    A.card + 1,
    IndexOf.option(A.indexOf),
    Get.option(A.get)
  )

  /** An instance for [[Either]]. */
  implicit def either[A, B](implicit A: Universe[A], B: Universe[B]): Universe[Either[A, B]] = of(
    Enumerate.either(A.enumerate, B.enumerate),
    A.card + B.card,
    IndexOf.either(A.indexOf, A.card, B.indexOf, B.card),
    Get.either(A.get, A.card, B.get, B.card)
  )

  /** An instance for [[Function1]]. */
  implicit def function1[A, B](implicit A: Finite[A], B: Universe[B]): Universe[A => B] = of(
    Enumerate.function1(A.indexOf, A.card, B.enumerate),
    B.card ** A.card,
    IndexOf.function1(A.enumerate, A.indexOf, A.card, B.indexOf, B.card),
    Get.function1(A.indexOf, A.card, B.get, B.card)
  )

  /** An instance for [[PartialFunction]]. It is same as [[Universe.map]] internally. */
  implicit def partialFunction[A, B](implicit A: Finite[A], B: Universe[B]): Universe[PartialFunction[A, B]] = of(
    Enumerate.map(A.enumerate, A.card, B.enumerate),
    (B.card + 1) ** A.card,
    IndexOf.partialFunction(A.enumerate, A.indexOf, A.card, B.indexOf, B.card),
    Get.partialFunction(A.get, A.card, B.get, B.card)
  )
}
