package codes.quine.labo.uchu

/** Natural number implementation and utilities. */
private[uchu] object NatImpl {
  // The following 3 types are components of a technique to
  // create a no-boxing new-type. It's copied from the
  // new-types lib by @alexknvl
  // For more detail see https://github.com/alexknvl/newtypes
  private[uchu] type Base = Any { type Nat$newtype }
  private[uchu] trait Tag extends Any

  /** A natural type. */
  type Type <: Base with Tag

  /** Creates a new natural number instance (unsafe). */
  private[uchu] def unsafeCreate(x: BigInt): Type = x.asInstanceOf[Type]

  /** Extracts a value from the natural number. */
  private[uchu] def extract(n: Type): BigInt = n.asInstanceOf[BigInt]

  /** Creates a new natural number instance. */
  def apply(x: BigInt): Nat = {
    require(x >= 0)
    unsafeCreate(x)
  }

  /** `0` */
  val Zero: Nat = Nat(0)

  /** `1` */
  val One: Nat = Nat(1)

  /** `2` */
  val Two: Nat = Nat(2)

  /** The maximum [[Int]] value. */
  val MaxInt: Nat = Nat(Int.MaxValue)

  /** The maximum safe number which can convert to [[Double]]. */
  val MaxSafeInt: Nat = Nat(1L << 52)

  /** Computes a square root. */
  def sqrt(n: Nat): Nat = {
    val x = n.value

    if (x < 2) return Nat(x)
    if (x < 16) return Nat(Math.floor(Math.sqrt(x.toDouble)).toLong)

    var x0: BigInt = -1
    var x1 =
      if (n < MaxSafeInt) BigInt(Math.floor(Math.sqrt(x.toDouble)).toLong) - 3
      else ((1: BigInt) << 52) - 2
    while (x0 != x1 && x0 != (x1 - 1)) {
      x0 = x1
      x1 = ((x / x0) + x0) >> 1
    }

    Nat(x0)
  }
}
