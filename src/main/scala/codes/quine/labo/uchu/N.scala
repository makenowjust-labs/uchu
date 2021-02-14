package codes.quine.labo.uchu

/** N is a natural number. */
final case class N(value: BigInt) extends Ordered[N] {
  require(value >= 0)

  /** Tests `n`th bit value. */
  def bit(n: Int): Boolean = value.testBit(n)

  /** Returns its bit size. */
  def bitSize: Int = value.bitLength

  /** Returns bit array. */
  def bits: LazyList[Boolean] = LazyList.range(0, bitSize).map(bit)

  /** Computes an addition. */
  def +(other: N): N = N(value + other.value)

  /** Computes an addition. */
  def +(other: Int): N = N(value + other)

  /** Computes a subtraction. */
  def -(other: N): N = N(value - other.value)

  /** Computes a subtraction. */
  def -(other: Int): N = N(value - other)

  /** Computes a multiplication. */
  def *(other: N): N = N(value * other.value)

  /** Computes a multiplication. */
  def *(other: Int): N = N(value * other)

  /** Computes a division. */
  def /(other: N): N = N(value / other.value)

  /** Computes a division. */
  def /(other: Int): N = N(value / other)

  /** Computes a division and reminder at once. */
  def /%(other: N): (N, N) = {
    val (div, mod) = value /% other.value
    (N(div), N(mod))
  }

  /** Computes a power. */
  def **(other: N): N = N(value.pow(other.toInt))

  /** Computes a power. */
  def **(other: Int): N = N(value.pow(other))

  /** Computes a bit-or. */
  def |(other: N): N = N(value | other.value)

  /** Computes a bit-or. */
  def |(other: Int): N = N(value | other)

  /** Computes a left bit-shift. */
  def <<(other: N): N = N(value << other.toInt)

  /** Computes a left bit-shift. */
  def <<(other: Int): N = N(value << other)

  def compare(other: N): Int = value.compare(other.value)

  /** Converts this into an integer if possible. */
  def toInt: Int = {
    if (this > N.MaxInt) throw new ArithmeticException
    else value.toInt
  }
}

/** Natural number utilities. */
object N {

  /** `0` */
  val Zero: N = N(0)

  /** `1` */
  val One: N = N(1)

  /** `2` */
  val Two: N = N(2)

  /** The maximum [[Int]] value. */
  val MaxInt: N = N(Int.MaxValue)

  /** The maximum safe number which can convert to [[Double]]. */
  val MaxSafeInt: N = N(1L << 52)

  /** Computes a square root. */
  def sqrt(n: N): N = {
    val x = n.value

    if (x < 2) return N(x)
    if (x < 16) return N(Math.floor(Math.sqrt(x.toDouble)).toLong)

    var x0: BigInt = -1
    var x1 =
      if (n < MaxSafeInt) BigInt(Math.floor(Math.sqrt(x.toDouble)).toLong) - 3
      else ((1: BigInt) << 52) - 2
    while (x0 != x1 && x0 != (x1 - 1)) {
      x0 = x1
      x1 = ((x / x0) + x0) >> 1
    }

    N(x0)
  }
}
