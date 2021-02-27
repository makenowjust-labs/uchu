package codes.quine.labo.uchu

/** Natural number implementation and utilities. */
private[uchu] object NatImpl {
  // The following 3 types are components of a technique to
  // create a no-boxing new-type. It's copied from the
  // new-types lib by @alexknvl
  // For more detail see https://github.com/alexknvl/newtypes
  private[uchu] type Base
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
  val Zero: Nat = unsafeCreate(0)

  /** `1` */
  val One: Nat = unsafeCreate(1)

  /** `2` */
  val Two: Nat = unsafeCreate(2)

  /** A number of [[Byte]] values. */
  val ByteSize: Nat = unsafeCreate(256)

  /** A number of [[Byte]] values. */
  val ShortSize: Nat = unsafeCreate(65536)

  /** A number of [[Int]] values. */
  val IntSize: Nat = unsafeCreate(1L << 32)

  /** A number of [[Long]] values. */
  val LongSize: Nat = unsafeCreate((1: BigInt) << 64)

  /** A number of [[Char]] values. */
  val CharSize: Nat = unsafeCreate(65536)

  /** The maximum [[Int]] value. */
  val MaxInt: Nat = unsafeCreate(Int.MaxValue)

  /** The maximum safe number which can convert to [[Double]]. */
  val MaxSafeInt: Nat = unsafeCreate((1: BigInt) << 52)

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

  /** Natural number operations. */
  private[uchu] class Ops(private val n: Nat) extends AnyVal with Ordered[Nat] {

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
