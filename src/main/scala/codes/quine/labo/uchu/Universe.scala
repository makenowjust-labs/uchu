package codes.quine.labo.uchu

/** Universe is a type-class for recursive enumerable types.
  *
  * It is derived by Haskell's [[https://hackage.haskell.org/package/universe universe]] library.
  */
trait Universe[A] extends Serializable {

  /** Enumerates all possible values of the type. */
  def universe: LazyList[A]
}

/** Universe utilities and instances. */
object Universe {

  /** Summon the instance. */
  @inline def apply[A](implicit A: Universe[A]): Universe[A] = A
}
