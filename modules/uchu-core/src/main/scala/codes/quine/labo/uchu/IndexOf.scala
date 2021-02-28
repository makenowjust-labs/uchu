package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

/** IndexOf is an indexer. */
trait IndexOf[A] extends (A => Nat) { self =>
  def apply(x: A): Nat

  /** Converts this by the given transformation. */
  def contramap[B](g: B => A): IndexOf[B] = IndexOf(x => self(g(x)))
}

/** IndexOf is utilities for indexing a value.
  *
  * The methods in this are used for implementing [[Universe.indexOf]].
  */
object IndexOf {

  /** Builds an indexer from a function. */
  def apply[A](f: A => Nat): IndexOf[A] = new IndexOf[A] {
    def apply(x: A): Nat = f(x)
  }

  /** Delays building an indexer. */
  def delay[A](i: => IndexOf[A]): IndexOf[A] = IndexOf(x => i(x))

  /** Indexes a [[Nothing]] value (an empty function). */
  def nothing: IndexOf[Nothing] = IndexOf[Nothing](_ => throw new IllegalArgumentException)

  /** Indexes an [[Unit]] value. */
  def unit: IndexOf[Unit] = IndexOf(_ => Nat.Zero)

  /** Indexes a [[Boolean]] value. */
  val boolean: IndexOf[Boolean] = IndexOf {
    case false => Nat.Zero
    case true  => Nat.One
  }

  /** Indexes a [[BigInt]] value. */
  val bigInt: IndexOf[BigInt] =
    IndexOf(x => if (x < 0) Nat((-x * 2) - 1) else Nat(x * 2))

  /** Indexes a [[Byte]] value. */
  val byte: IndexOf[Byte] = IndexOf(x => bigInt(BigInt(x)))

  /** Indexes a [[Short]] value. */
  val short: IndexOf[Short] = IndexOf(x => bigInt(BigInt(x)))

  /** Indexes an [[Int]] value. */
  val int: IndexOf[Int] = IndexOf(x => bigInt(BigInt(x)))

  /** Indexes a [[Long]] value. */
  val long: IndexOf[Long] = IndexOf(x => bigInt(BigInt(x)))

  /** Indexes a [[Char]] value. */
  val char: IndexOf[Char] = IndexOf(x => Nat(x.toInt))

  /** Indexes a [[String]] value. */
  val string: IndexOf[String] = {
    val iList = seq(char, Small(Nat.CharSize))
    IndexOf(x => iList(x.toList))
  }

  /** Indexes a pair of values. */
  def tuple2[A, B](ix: IndexOf[A], cx: Card, iy: IndexOf[B], cy: Card): IndexOf[(A, B)] = {
    def finFin(nx: Nat, ny: Nat): IndexOf[(A, B)] = {
      val (min, max, landscape) = if (nx < ny) (nx, ny, false) else (ny, nx, true)
      val upper = min * (min + 1) / 2
      val diagonals = min * (max - min)
      IndexOf { case (x, y) =>
        val (kx, ky) = (ix(x), iy(y))
        val d = kx + ky
        if (d < min) d * (d + 1) / 2 + ky
        else if (d < max) upper + (d - min) * min + (if (landscape) ky else nx - kx - 1)
        else {
          val j = d - max
          upper + diagonals + ((min - j) + (min - 1)) * j / 2 + (nx - kx - 1)
        }
      }
    }

    def finInf(n: Nat, landscape: Boolean): IndexOf[(A, B)] = {
      val upper = n * (n + 1) / 2
      IndexOf { case (x, y) =>
        val (kx, ky) = (ix(x), iy(y))
        val d = kx + ky
        if (d < n) d * (d + 1) / 2 + (d - kx)
        else upper + (d - n) * n + (if (landscape) ky else n - kx - 1)
      }
    }

    def infInf: IndexOf[(A, B)] =
      IndexOf { case (x, y) =>
        val (kx, ky) = (ix(x), iy(y))
        val d = kx + ky
        d * (d + 1) / 2 + (d - kx)
      }

    (cx, cy) match {
      case (Small(nx), Small(ny)) => finFin(nx, ny)
      case (Small(n), _)          => finInf(n, false)
      case (_, Small(n))          => finInf(n, true)
      case (_, _)                 => infInf
    }
  }

  /** Indexes a list. */
  def seq[A](i: IndexOf[A], c: Card): IndexOf[Seq[A]] =
    new IndexOf[Seq[A]] { iSeq =>
      private[this] val iCons = tuple2(i, c, iSeq, Inf)
      def apply(xs: Seq[A]): Nat = xs match {
        case Nil     => Nat.Zero
        case x +: xs => iCons((x, xs)) + 1
      }
    }

  /** Indexes a set. */
  def set[A](i: IndexOf[A]): IndexOf[Set[A]] =
    IndexOf(_.map(i).foldLeft(Nat(0)) { case (acc, k) => acc | (Nat.One << k) })

  /** Indexes a map. */
  def map[A, B](ix: IndexOf[A], cx: Fin, iy: IndexOf[B], cy: Card): IndexOf[Map[A, B]] = {
    val cListLeN = Card.sumOfGeometric(One, cy + 1, cx)
    val iCons = tuple2(listLeN(option(iy), cy + 1, cx - 1), cListLeN, iy, cy)
    IndexOf { map =>
      if (map.isEmpty) Nat.Zero
      else {
        val imap = map.map { case (k, v) => (ix(k), v) }
        val (max, maxY) = imap.maxBy(_._1)
        val list = Enumerate.to(max).map(imap.get).toList
        iCons((list, maxY)) + 1
      }
    }
  }

  /** Indexes a function. */
  def function1[A, B](xs: LazyList[A], ix: IndexOf[A], cx: Fin, iy: IndexOf[B], cy: Card): IndexOf[A => B] = {
    val iMap = map(ix, cx, IndexOf((y: B) => iy(y) - 1), cy - 1)
    IndexOf {
      case map: Map[A, B] =>
        // We assumes an index of the default value of `map` is `0`.
        iMap(map.collect { case (x, y) if iy(y) != Nat.Zero => (x, y) })
      case f =>
        iMap(xs.zip(xs.map(f)).collect { case (x, y) if iy(y) != Nat.Zero => (x, y) }.toMap)
    }
  }

  /** Indexes a partial function. */
  def partialFunction[A, B](
      xs: LazyList[A],
      ix: IndexOf[A],
      cx: Fin,
      iy: IndexOf[B],
      cy: Card
  ): IndexOf[PartialFunction[A, B]] = {
    val iMap = map(ix, cx, iy, cy)
    IndexOf { pf =>
      val map = xs.flatMap(x => pf.lift(x).map((x, _))).toMap
      iMap(map)
    }
  }

  /** Indexes an optional value. */
  def option[A](i: IndexOf[A]): IndexOf[Option[A]] = IndexOf {
    case None    => Nat.Zero
    case Some(x) => i(x) + 1
  }

  /** Indexes an either value. */
  def either[A, B](ix: IndexOf[A], cx: Card, iy: IndexOf[B], cy: Card): IndexOf[Either[A, B]] =
    IndexOf {
      case Left(x) =>
        cy match {
          case Small(ny) =>
            val kx = ix(x)
            if (kx >= ny) ny * 2 + (kx - ny) else kx * 2
          case _ => ix(x) * 2
        }
      case Right(y) =>
        cx match {
          case Small(nx) =>
            val ky = iy(y)
            if (ky >= nx) nx * 2 + (ky - nx) else ky * 2 + 1
          case _ => iy(y) * 2 + 1
        }
    }

  /** Indexes a list which sizes up to the given parameter. */
  private def listLeN[A](i: IndexOf[A], c: Card, size: Fin): IndexOf[List[A]] =
    if (size.isZero) IndexOf(_ => Nat.Zero)
    else {
      val cListLeN = Card.sumOfGeometric(One, c, size)
      val iCons = tuple2(i, c, delay(listLeN(i, c, size - 1)), cListLeN)
      IndexOf {
        case Nil     => Nat.Zero
        case x :: xs => iCons((x, xs)) + 1
      }
    }
}
