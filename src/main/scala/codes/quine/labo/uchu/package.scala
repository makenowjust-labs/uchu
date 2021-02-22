package codes.quine.labo

package object uchu {

  /** Nat is a natural number type. */
  type Nat = NatImpl.Type

  /** Natural number utilities.. */
  lazy val Nat: NatImpl.type = NatImpl

  /** Natural number operations. */
  implicit class NatOps(private val n: Nat) extends AnyVal with Ordered[Nat] {

    /** A value of this natural number. */
    def value: BigInt = Nat.extract(n)

    /** Tests `n`th bit value. */
    def bit(n: Int): Boolean = value.testBit(n)

    /** Returns its bit size. */
    def bitSize: Int = value.bitLength

    /** Returns bit array. */
    def bits: LazyList[Boolean] = LazyList.range(0, bitSize).map(bit)

    /** Computes an addition. */
    def +(other: Nat): Nat = Nat.unsafeCreate(value + other.value)

    /** Computes an addition. */
    def +(other: Int): Nat = Nat(value + other)

    /** Computes a subtraction. */
    def -(other: Nat): Nat = Nat(value - other.value)

    /** Computes a subtraction. */
    def -(other: Int): Nat = Nat(value - other)

    /** Computes a multiplication. */
    def *(other: Nat): Nat = Nat.unsafeCreate(value * other.value)

    /** Computes a multiplication. */
    def *(other: Int): Nat = Nat(value * other)

    /** Computes a division. */
    def /(other: Nat): Nat = Nat.unsafeCreate(value / other.value)

    /** Computes a division. */
    def /(other: Int): Nat = Nat(value / other)

    /** Computes a division and reminder at once. */
    def /%(other: Nat): (Nat, Nat) = {
      val (div, mod) = value /% other.value
      (Nat.unsafeCreate(div), Nat.unsafeCreate(mod))
    }

    /** Computes a power. */
    def **(other: Nat): Nat = Nat.unsafeCreate(value.pow(other.toInt))

    /** Computes a power. */
    def **(other: Int): Nat = Nat.unsafeCreate(value.pow(other))

    /** Computes a bit-or. */
    def |(other: Nat): Nat = Nat.unsafeCreate(value | other.value)

    /** Computes a bit-or. */
    def |(other: Int): Nat = Nat.unsafeCreate(value | other)

    /** Computes a left bit-shift. */
    def <<(other: Nat): Nat = Nat.unsafeCreate(value << other.toInt)

    /** Computes a left bit-shift. */
    def <<(other: Int): Nat = Nat.unsafeCreate(value << other)

    def compare(other: Nat): Int = value.compare(other.value)

    /** Converts this into an integer if possible. */
    def toInt: Int = {
      if (this > Nat.MaxInt) throw new ArithmeticException
      else value.toInt
    }
  }
}
