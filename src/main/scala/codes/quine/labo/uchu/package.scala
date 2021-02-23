package codes.quine.labo

import scala.language.implicitConversions

package object uchu {

  /** Nat is a natural number type. */
  type Nat = NatImpl.Type

  /** Natural number utilities.. */
  val Nat: NatImpl.type = NatImpl

  /** Implicit conversion for [[Nat]]. */
  implicit def NatOps(n: Nat): NatOps = new NatOps(n)

  /** An alias to [[Nat.Ops]]. */
  type NatOps = Nat.Ops
}
