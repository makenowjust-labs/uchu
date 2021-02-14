package codes.quine.labo.uchu

import codes.quine.labo.uchu.Cardinality._

/** IndexOf is an indexer. */
trait IndexOf[A] extends (A => BigInt) {
  def apply(x: A): BigInt
}

/** IndexOf is utilities for indexing a value.
  *
  * The methods in this are used for implementing [[Universe.indexOf]].
  */
object IndexOf {

  /** Builds an indexer from a function. */
  def apply[A](f: A => BigInt): IndexOf[A] = new IndexOf[A] {
    def apply(x: A): BigInt = f(x)
  }

  /** Indexes a [[Nothing]] value (an empty function). */
  def nothing: IndexOf[Nothing] = IndexOf[Nothing](_ => throw new IllegalArgumentException)

  /** Indexes an [[Unit]] value. */
  def unit: IndexOf[Unit] = IndexOf(_ => 0)

  /** Indexes a [[Boolean]] value. */
  val boolean: IndexOf[Boolean] = IndexOf {
    case false => 0
    case true  => 1
  }

  /** Indexes a [[BigInt]] value. */
  val bigInt: IndexOf[BigInt] =
    IndexOf(x => if (x < 0) (-x * 2) - 1 else x * 2)

  /** Indexes a [[Byte]] value. */
  val byte: IndexOf[Byte] = IndexOf(x => bigInt(BigInt(x)))

  /** Indexes a [[Short]] value. */
  val short: IndexOf[Short] = IndexOf(x => bigInt(BigInt(x)))

  /** Indexes an [[Int]] value. */
  val int: IndexOf[Int] = IndexOf(x => bigInt(BigInt(x)))

  /** Indexes a [[Long]] value. */
  val long: IndexOf[Long] = IndexOf(x => bigInt(BigInt(x)))

  /** Indexes a pair of values. */
  def tuple2[A, B](
      indexOfX: IndexOf[A],
      cardX: Cardinality,
      indexOfY: IndexOf[B],
      cardY: Cardinality
  ): IndexOf[(A, B)] = {
    def finFin(n: BigInt, m: BigInt): IndexOf[(A, B)] = {
      val (min, max, landscape) = if (n < m) (n, m, false) else (m, n, true)
      val upper = min * (min + 1) / 2
      val diagonals = min * (max - min)
      IndexOf { case (x, y) =>
        val (i, j) = (indexOfX(x), indexOfY(y))
        val d = i + j
        if (d < min) d * (d + 1) / 2 + j
        else if (d < max) upper + (d - min) * min + (if (landscape) j else n - i - 1)
        else {
          val k = d - max
          upper + diagonals + ((min - k) + (min - 1)) * k / 2 + (n - i - 1)
        }
      }
    }

    def finInf(n: BigInt, landscape: Boolean): IndexOf[(A, B)] = {
      val upper = n * (n + 1) / 2
      IndexOf { case (x, y) =>
        val (i, j) = (indexOfX(x), indexOfY(y))
        val d = i + j
        if (d < n) d * (d + 1) / 2 + (d - i)
        else upper + (d - n) * n + (if (landscape) j else n - i - 1)
      }
    }

    def infInf: IndexOf[(A, B)] =
      IndexOf { case (x, y) =>
        val (i, j) = (indexOfX(x), indexOfY(y))
        val d = i + j
        d * (d + 1) / 2 + (d - i)
      }

    (cardX, cardY) match {
      case (Fin(n), Fin(m)) => finFin(n, m)
      case (Fin(n), Inf)    => finInf(n, false)
      case (Inf, Fin(n))    => finInf(n, true)
      case (Inf, Inf)       => infInf
    }
  }

  /** Indexes a list. */
  def list[A](indexOf: IndexOf[A], card: Cardinality): IndexOf[List[A]] =
    new IndexOf[List[A]] { indexOfListX =>
      private[this] val t = tuple2(indexOf, card, indexOfListX, Inf)
      def apply(xs: List[A]): BigInt = xs match {
        case Nil     => 0
        case x :: xs => 1 + t((x, xs))
      }
    }

  /** Indexes a set. */
  def set[A](indexOf: IndexOf[A], card: Fin): IndexOf[Set[A]] = {
    val s = sized(boolean, Fin(2), card.size - 1)
    IndexOf { x =>
      if (x.isEmpty) 0
      else {
        val is = x.map(indexOf)
        val max = is.max
        val list = List.unfold(0: BigInt)(i => if (i >= max) None else Some((is.contains(i), i + 1)))
        1 + s(list)
      }
    }
  }

  /** Indexes a map. */
  def map[A, B](indexOfX: IndexOf[A], cardX: Fin, indexOfY: IndexOf[B], cardY: Cardinality): IndexOf[Map[A, B]] = {
    val cardSized = cardY match {
      case Fin(n) => Fin((1 - (n + 1).pow(cardX.toInt)) / (1 - (n + 1)))
      case Inf    => Inf
    }
    val t = tuple2(sized(option(indexOfY), cardY + 1, cardX.size - 1), cardSized, indexOfY, cardY)
    IndexOf { map =>
      if (map.isEmpty) 0
      else {
        val imap = map.map { case (k, v) => (indexOfX(k), v) }
        val (max, maxX) = imap.maxBy(_._1)
        val list = List.unfold(0: BigInt)(i => if (i >= max) None else Some((imap.get(i), i + 1)))
        1 + t((list, maxX))
      }
    }
  }

  /** Indexes a function. */
  def function1[A, B](xs: LazyList[A], cardX: Fin, indexOfY: IndexOf[B], cardY: Cardinality): IndexOf[A => B] = {
    val l = listN(indexOfY, cardY, cardX.size)
    IndexOf { f => l(xs.map(f).toList) }
  }

  /** Indexes a partial function. */
  def partialFunction[A, B](
      indexOfX: IndexOf[A],
      xs: LazyList[A],
      cardX: Fin,
      indexOfY: IndexOf[B],
      cardY: Cardinality
  ): IndexOf[PartialFunction[A, B]] = {
    val indexOfMap = map(indexOfX, cardX, indexOfY, cardY)
    IndexOf { pf =>
      val map = xs.flatMap(x => pf.lift(x).map((x, _))).toMap
      indexOfMap(map)
    }
  }

  /** Indexes an optional value. */
  def option[A](indexOf: IndexOf[A]): IndexOf[Option[A]] = IndexOf {
    case None    => 0
    case Some(x) => indexOf(x) + 1
  }

  /** Indexes an either value. */
  def either[A, B](
      indexOfX: IndexOf[A],
      cardX: Cardinality,
      indexOfY: IndexOf[B],
      cardY: Cardinality
  ): IndexOf[Either[A, B]] =
    IndexOf {
      case Left(x) =>
        cardY match {
          case Fin(m) =>
            val i = indexOfX(x)
            if (i >= m) m * 2 + (i - m) else i * 2
          case Inf => indexOfX(x) * 2
        }
      case Right(y) =>
        cardX match {
          case Fin(n) =>
            val j = indexOfY(y)
            if (j >= n) n * 2 + (j - n) else j * 2 + 1
          case Inf => indexOfY(y) * 2 + 1
        }
    }

  /** Indexes a list which size up to the given parameter. */
  private def sized[A](indexOf: IndexOf[A], card: Cardinality, size: BigInt): IndexOf[List[A]] =
    if (size <= 0) IndexOf(_ => 0)
    else {
      val cardSized = card match {
        case Fin(n) => Fin((1 - n.pow(Fin(size).toInt)) / (1 - n))
        case Inf    => Inf
      }
      val t = tuple2(indexOf, card, IndexOf[List[A]](xs => sized(indexOf, card, size - 1)(xs)), cardSized)
      IndexOf {
        case Nil     => 0
        case x :: xs => 1 + t((x, xs))
      }
    }

  /** Indexes a list which size to the given parameter. */
  private def listN[A](indexOf: IndexOf[A], card: Cardinality, size: BigInt): List[A] => BigInt =
    if (size <= 0) IndexOf(_ => 0)
    else {
      val cardSized = card match {
        case Fin(n) => Fin(n.pow(Fin(size - 1).toInt))
        case Inf    => Inf
      }
      val t = tuple2(indexOf, card, IndexOf[List[A]](xs => listN(indexOf, card, size - 1)(xs)), cardSized)
      IndexOf {
        case Nil     => throw new IllegalArgumentException // unreachable
        case x :: xs => t((x, xs))
      }
    }
}
