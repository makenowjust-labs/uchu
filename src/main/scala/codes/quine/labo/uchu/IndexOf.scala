package codes.quine.labo.uchu

import Cardinality._

/** IndexOf is utilities for indexing a value.
  *
  * The methods in this are used for implementing [[Universe.indexOf]].
  */
private[uchu] object IndexOf {

  /** Indexes a [[Boolean]] value. */
  val boolean: Boolean => BigInt =
    (x: Boolean) => if (!x) 0 else 1

  /** Indexes a [[BigInt]] value. */
  val bigInt: BigInt => BigInt =
    (x: BigInt) => if (x < 0) (-x * 2) - 1 else x * 2

  /** Indexes a [[Byte]] value. */
  val byte: Byte => BigInt = (x: Byte) => bigInt(BigInt(x))

  /** Indexes a [[Short]] value. */
  val short: Short => BigInt = (x: Short) => bigInt(BigInt(x))

  /** Indexes an [[Int]] value. */
  val int: Int => BigInt = (x: Int) => bigInt(BigInt(x))

  /** Indexes a [[Long]] value. */
  val long: Long => BigInt = (x: Long) => bigInt(BigInt(x))

  /** Indexes a pair of values. */
  def tuple2[A, B](a: A => BigInt, aCard: Cardinality, b: B => BigInt, bCard: Cardinality): ((A, B)) => BigInt = {
    def finFin(n: BigInt, m: BigInt): ((A, B)) => BigInt = {
      val (min, max, landscape) = if (n < m) (n, m, false) else (m, n, true)
      val upper = min * (min + 1) / 2
      val diagonals = min * (max - min)
      (xy: (A, B)) => {
        val (i, j) = (a(xy._1), b(xy._2))
        val d = i + j
        if (d < min) d * (d + 1) / 2 + j
        else if (d < max) upper + (d - min) * min + (if (landscape) j else n - i - 1)
        else {
          val k = d - max
          upper + diagonals + ((min - k) + (min - 1)) * k / 2 + (n - i - 1)
        }
      }
    }

    def finInf(n: BigInt, landscape: Boolean): ((A, B)) => BigInt = {
      val upper = n * (n + 1) / 2
      (xy: (A, B)) => {
        val (i, j) = (a(xy._1), b(xy._2))
        val d = i + j
        if (d < n) d * (d + 1) / 2 + (d - i)
        else upper + (d - n) * n + (if (landscape) j else n - i - 1)
      }
    }

    def infInf: ((A, B)) => BigInt =
      (xy: (A, B)) => {
        val (i, j) = (a(xy._1), b(xy._2))
        val d = i + j
        d * (d + 1) / 2 + (d - i)
      }

    (aCard, bCard) match {
      case (Fin(n), Fin(m)) => finFin(n, m)
      case (Fin(n), Inf)    => finInf(n, false)
      case (Inf, Fin(n))    => finInf(n, true)
      case (Inf, Inf)       => infInf
    }
  }

  /** Indexes a list. */
  def list[A](a: A => BigInt, aCard: Cardinality): List[A] => BigInt =
    new (List[A] => BigInt) { rec =>
      private[this] val t = tuple2(a, aCard, rec, Inf)
      def apply(xs: List[A]): BigInt = xs match {
        case Nil => 0
        case x :: xs => 1 + t((x, xs))
      }
    }

  /** Indexes a set. */
  def set[A](a: A => BigInt, aCard: Fin): Set[A] => BigInt = {
    val s = sized(boolean, Fin(2), aCard.size - 1)
    (x: Set[A]) =>
      if (x.isEmpty) 0
      else {
        val is = x.map(a)
        val max = is.max
        val list = List.unfold(0: BigInt)(i => if (i >= max) None else Some((is.contains(i), i + 1)))
        1 + s(list)
      }
  }

  /** Indexes a map. */
  def map[A, B](a: A => BigInt, aCard: Fin, b: B => BigInt, bCard: Cardinality): Map[A, B] => BigInt = {
    val sizedCard = bCard match {
      case Fin(n) => Fin((1 - (n + 1).pow(aCard.toInt)) / (1 - (n + 1)))
      case Inf    => Inf
    }
    val t = tuple2(sized(option(b), bCard + 1, aCard.size - 1), sizedCard, b, bCard)
    (x: Map[A, B]) =>
      if (x.isEmpty) 0
      else {
        val is = x.map { case (k, v) => (a(k), v) }.toMap
        val max = is.keySet.max
        val list = List.unfold(0: BigInt)(i => if (i >= max) None else Some((is.get(i), i + 1)))
        1 + t((list, is(max)))
      }
  }

  /** Indexes a function. */
  def function1[A, B](xs: LazyList[A], aCard: Fin, b: B => BigInt, bCard: Cardinality): (A => B) => BigInt = {
    val l = listN(b, bCard, aCard.size)
    (f: A => B) => l(xs.map(f).toList)
  }

  /** Indexes a partial function. */
  def partialFunction[A, B](a: A => BigInt, xs: LazyList[A], aCard: Fin, b: B => BigInt, bCard: Cardinality): PartialFunction[A, B] => BigInt = {
    val sizedCard = bCard match {
      case Fin(n) => Fin((1 - (n + 1).pow(aCard.toInt)) / (1 - (n + 1)))
      case Inf    => Inf
    }
    val t = tuple2(sized(option(b), bCard + 1, aCard.size - 1), sizedCard, b, bCard)
    (f: PartialFunction[A, B]) => {
      val x = xs.flatMap(x => f.lift(x).map((x, _))).toMap
      if (x.isEmpty) 0
      else {
        val is = x.map { case (k, v) => (a(k), v) }.toMap
        val max = is.keySet.max
        val list = List.unfold(0: BigInt)(i => if (i >= max) None else Some((is.get(i), i + 1)))
        1 + t((list, is(max)))
      }
    }
  }

  /** Indexes an optional value. */
  def option[A](a: A => BigInt): Option[A] => BigInt = {
    case None => 0
    case Some(x) => a(x) + 1
  }

  /** Indexes an either value. */
  def either[A, B](a: A => BigInt, aCard: Cardinality, b: B => BigInt, bCard: Cardinality): Either[A, B] => BigInt = {
    case Left(x) => bCard match {
      case Fin(m) =>
        val i = a(x)
        if (i >= m) m * 2 + (i - m) else i * 2
      case Inf => a(x) * 2
    }
    case Right(y) => aCard match {
      case Fin(n) =>
        val j = b(y)
        if (j >= n) n * 2 + (j - n) else j * 2 + 1
      case Inf => b(y) * 2 + 1
    }
  }

  /** Indexes a list which size up to the given parameter. */
  private[uchu] def sized[A](a: A => BigInt, aCard: Cardinality, size: BigInt): List[A] => BigInt =
    if (size <= 0) Function.const(0)
    else {
      val sizedCard = aCard match {
        case Fin(n) => Fin((1 - n.pow(Fin(size).toInt)) / (1 - n))
        case Inf => Inf
      }
      val t = tuple2(a, aCard, (xs: List[A]) => sized(a, aCard, size - 1)(xs), sizedCard)

      {
        case Nil => 0
        case x :: xs => 1 + t((x, xs))
      }
    }

  /** Indexes a list which size to the given parameter. */
  private[uchu] def listN[A](a: A => BigInt, aCard: Cardinality, size: BigInt): List[A] => BigInt =
    if (size <= 0) Function.const(0)
    else {
      val sizedCard = aCard match {
        case Fin(n) => Fin(n.pow(Fin(size - 1).toInt))
        case Inf => Inf
      }
      val t = tuple2(a, aCard, (xs: List[A]) => listN(a, aCard, size - 1)(xs), sizedCard)

      {
        case Nil => 0
        case x :: xs => t((x, xs))
      }
    }
}
