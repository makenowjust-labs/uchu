package codes.quine.labo.uchu

import codes.quine.labo.uchu.Cardinality._

/** Cardinality is a cardinality of an enumerable type. */
sealed abstract class Cardinality extends Product with Serializable { self =>

  /** Computes the addition of two cardinalities. */
  def +(other: Cardinality): Cardinality = (self, other) match {
    case (Fin(n), Fin(m)) => Fin(n + m)
    case (Inf, _)         => Inf
    case (_, Inf)         => Inf
  }

  /** Computes the addition of this cardinality and the integer value. */
  def +(other: BigInt): Cardinality = self match {
    case Fin(n) => Fin(n + other)
    case Inf    => Inf
  }

  /** Computes the multiplication of two cardinalities. */
  def *(other: Cardinality): Cardinality = (self, other) match {
    case (Fin(n), Fin(m)) => Fin(n * m)
    case (Inf, _)         => Inf
    case (_, Inf)         => Inf
  }

  /** Computes this to the power of the other cardinality. */
  def **(other: Fin): Cardinality = self match {
    case Fin(n) => Fin(n.pow(other.toInt))
    case Inf    => Inf
  }
}

/** Cardinality kinds. */
object Cardinality {

  /** A finite cardinality. */
  final case class Fin(size: BigInt) extends Cardinality {
    require(size >= 0)

    def +(other: Fin): Fin = super.+(other).asInstanceOf[Fin]
    def +(other: Inf): Inf = super.+(other).asInstanceOf[Inf]
    override def +(other: BigInt): Fin = super.+(other).asInstanceOf[Fin]
    def *(other: Fin): Fin = super.*(other).asInstanceOf[Fin]
    def *(other: Inf): Inf = super.*(other).asInstanceOf[Inf]
    override def **(other: Fin): Fin = super.**(other).asInstanceOf[Fin]

    /** Converts this cardinality to an integer value.
      * If is is too large, it throws [[ArithmeticException]].
      */
    private[uchu] def toInt: Int =
      if (size > Int.MaxValue) throw new ArithmeticException
      else size.toInt
  }

  /** An infinite cardinality. */
  case object Inf extends Cardinality

  /** An infinite cardinality type. */
  type Inf = Inf.type
}
