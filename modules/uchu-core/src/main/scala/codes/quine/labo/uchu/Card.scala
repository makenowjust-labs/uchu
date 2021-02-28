package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

/** Card is a cardinality of an enumerable type. */
sealed abstract class Card extends Product with Serializable with Ordered[Card] { self =>

  /** Checks this cardinality is zero or not. */
  def isZero: Boolean = self == Zero

  /** Computes the addition of two cardinalities. */
  def +(other: Card): Card = (self, other) match {
    case (Small(n), Small(m)) => Fin(n + m)
    case (TooLarge, Small(_)) => TooLarge
    case (Small(_), TooLarge) => TooLarge
    case (TooLarge, TooLarge) => TooLarge
    case (Inf, _)             => Inf
    case (_, Inf)             => Inf
  }

  /** Computes the addition of this cardinality and the integer value. */
  def +(other: Int): Card = self match {
    case Small(n) => Fin(n + other)
    case TooLarge => TooLarge
    case Inf      => Inf
  }

  /** Computes the subtraction of this cardinality and the integer value. */
  def -(other: Int): Card = self match {
    case Small(n) => Fin(n - other)
    case TooLarge => TooLarge
    case Inf      => Inf
  }

  /** Computes the multiplication of two cardinalities. */
  def *(other: Card): Card = (self, other) match {
    case (Small(n), Small(m)) => Fin(n * m)
    case (Zero, _)            => Zero
    case (_, Zero)            => Zero
    case (TooLarge, Small(_)) => TooLarge
    case (Small(_), TooLarge) => TooLarge
    case (TooLarge, TooLarge) => TooLarge
    case (Inf, _)             => Inf
    case (_, Inf)             => Inf
  }

  /** Computes this to the power of the other cardinality. */
  def **(other: Fin): Card = (self, other) match {
    case (Zero, _)            => Zero
    case (One, _)             => One
    case (_, Zero)            => One
    case (x, One)             => x
    case (Small(n), Small(m)) => Fin(n ** m)
    case (TooLarge, Small(_)) => TooLarge
    case (Small(_), TooLarge) => TooLarge
    case (TooLarge, TooLarge) => TooLarge
    case (Inf, _)             => Inf
  }

  /** Computes this to the power of the other number. */
  def **(other: Nat): Card = self ** Small(other)

  def compare(other: Card): Int = (self, other) match {
    case (Small(n), Small(m)) => n.compare(m)
    case (Small(_), _)        => -1
    case (_, Small(_))        => 1
    case (TooLarge, TooLarge) => 0
    case (TooLarge, _)        => -1
    case (_, TooLarge)        => 1
    case (Inf, Inf)           => 0
  }
}

/** Cardinality kinds. */
object Card {

  /** Computes a sum of `n` terms of geometric series
    * with the initial value `a` and the common ration `r`.
    */
  def sumOfGeometric(a: Fin, r: Card, n: Fin): Card =
    if (r == One) a * n
    else {
      (r, r ** n) match {
        case (Small(r), Small(rn)) => a * Small(Nat((1 - rn.value) / (1 - r.value)))
        case (_, rn)               => a * rn
      }
    }

  /** A finite cardinality. */
  sealed abstract class Fin extends Card {
    def +(other: Fin): Fin = super.+(other).asInstanceOf[Fin]
    def +(other: Inf): Inf = super.+(other).asInstanceOf[Inf]
    override def +(other: Int): Fin = super.+(other).asInstanceOf[Fin]
    override def -(other: Int): Fin = super.-(other).asInstanceOf[Fin]
    def *(other: Fin): Fin = super.*(other).asInstanceOf[Fin]
    def *(other: Inf): Inf = super.*(other).asInstanceOf[Inf]
    override def **(other: Fin): Fin = super.**(other).asInstanceOf[Fin]
  }

  /** Fin utilities. */
  object Fin {

    /** Creates a new instance with covering [[ArithmeticException]]. */
    def apply(size: => Nat): Fin =
      try Small(size)
      catch { case _: ArithmeticException => TooLarge }
  }

  /** A zero cardinality. */
  val Zero: Small = Small(Nat.Zero)

  /** A one cardinality. */
  val One: Small = Small(Nat.One)

  /** A two cardinality. */
  val Two: Small = Small(Nat.Two)

  /** A finite cardinality with a size. */
  final case class Small(size: Nat) extends Fin

  /** Small utilities. */
  object Small {

    /** Creates a new instance from a [[BigInt]]. */
    def apply(n: BigInt): Small = Small(Nat(n))
  }

  /** A finite cardinality without a size. */
  case object TooLarge extends Fin

  /** An infinite cardinality.
    *
    * It means not only infinite but also enumerable.
    */
  case object Inf extends Card

  /** An infinite cardinality type. */
  type Inf = Inf.type
}
