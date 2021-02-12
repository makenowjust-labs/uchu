package codes.quine.labo.uchu

/** Finite is a type-class for finite types.
  *
  * It is derived by Haskell's [[https://hackage.haskell.org/package/universe universe]] library.
  */
trait Finite[A] extends Universe[A] {

  /** A cardinality of the type. */
  def cardinality: BigInt
}

/** Finite utilities and instances. */
object Finite {

  /** Summon the instance. */
  @inline def apply[A](implicit A: Finite[A]): Finite[A] = A
}
