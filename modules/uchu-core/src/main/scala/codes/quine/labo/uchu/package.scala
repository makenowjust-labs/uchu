package codes.quine.labo

package object uchu {

  // A dirty hack to correct the implicit conversion for `+` method.
  // See https://stackoverflow.com/questions/43966387/shadow-any2stringadd-when-implicitly-converting-symbol
  private[uchu] object any2stringadd

  /** Nat is a natural number type. */
  type Nat = NatImpl.Type

  /** Natural number utilities.. */
  val Nat: NatImpl.type = NatImpl
}
