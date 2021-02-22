package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

/** Get is a getter. */
trait Get[A] extends (Nat => Option[A]) {
  def apply(n: Nat): Option[A]
}

/** Get is utilities for getting a value from an index.
  *
  * The methods in this are used for implementing [[Universe#get]].
  */
object Get {

  /** Builds a getter from a function. */
  def apply[A](f: Nat => Option[A]): Get[A] = new Get[A] {
    def apply(k: Nat): Option[A] = f(k)
  }

  /** Delays building a getter. */
  def delay[A](g: => Get[A]): Get[A] = Get(k => g(k))

  /** An empty getter. */
  val nothing: Get[Nothing] = Get[Nothing](_ => None)

  /** Gets a [[Unit]] value from an index. */
  val unit: Get[Unit] = Get(k => if (k == Nat.Zero) Some(()) else None)

  /** Gets a [[Boolean]] value from an index. */
  val boolean: Get[Boolean] = Get { k =>
    if (k == Nat.Zero) Some(false)
    else if (k == Nat.One) Some(true)
    else None
  }

  /** Gets a [[BigInt]] value from an index. */
  val bigInt: Get[BigInt] =
    Get(k => Some(if (k.value % 2 == 0) k.value / 2 else -((k.value + 1) / 2)))

  /** Gets a [[Byte]] value from an index. */
  val byte: Get[Byte] =
    Get(k => if (k < Nat.Two ** 8) bigInt(k).map(_.toByte) else None)

  /** Gets a [[Short]] value from an index. */
  val short: Get[Short] =
    Get(k => if (k < Nat.Two ** 16) bigInt(k).map(_.toShort) else None)

  /** Gets a [[Int]] value from an index. */
  val int: Get[Int] =
    Get(k => if (k < Nat.Two ** 32) bigInt(k).map(_.toInt) else None)

  /** Gets a [[Long]] value from an index. */
  val long: Get[Long] =
    Get(k => if (k < Nat.Two ** 64) bigInt(k).map(_.toLong) else None)

  /** Gets a pair of values from an index. */
  def tuple2[A, B](gx: Get[A], cx: Card, gy: Get[B], cy: Card): Get[(A, B)] = {
    val c = cx * cy

    def finFin(nx: Nat, ny: Nat): Get[(A, B)] = {
      val (min, max, landscape) = if (nx < ny) (nx, ny, false) else (ny, nx, true)
      val upper = min * (min + 1) / 2
      val diagonals = min * (max - min)
      Get { k =>
        if (k < upper) {
          val d = diagonal(k)
          val ky = k - d * (d + 1) / 2
          val kx = d - ky
          Some((gx(kx).get, gy(ky).get))
        } else if (k < upper + diagonals) {
          val (d, r) = (k - upper) /% min
          val (kx, ky) = if (landscape) (ny - r + d, r) else (nx - 1 - r, r + d + 1)
          Some((gx(kx).get, gy(ky).get))
        } else if (Small(k) < c) {
          val r = k - upper - diagonals
          val j = min - 1 - diagonal((min - 1) * min / 2 - r - 1) - 1
          val jx = (r - ((min - j) + (min - 1)) * j / 2)
          val kx = nx - 1 - jx
          val ky = (if (landscape) Nat.One else (max - min) + 1) + j + jx
          Some(gx(kx).get, gy(ky).get)
        } else None
      }
    }

    def finInf(n: Nat, landscape: Boolean): Get[(A, B)] = {
      val upper = n * (n + 1) / 2
      Get { k =>
        if (k < upper) {
          val d = diagonal(k)
          val ky = k - d * (d + 1) / 2
          val kx = d - ky
          Some((gx(kx).get, gy(ky).get))
        } else if (Small(k) < c) {
          val (d, r) = (k - upper) /% n
          val (kx, ky) = if (landscape) (n - r + d, r) else (n - 1 - r, r + d + 1)
          Some((gx(kx).get, gy(ky).get))
        } else None
      }
    }

    def infInf: Get[(A, B)] = Get { k =>
      val d = diagonal(k)
      val ky = k - d * (d + 1) / 2
      val kx = d - ky
      Some((gx(kx).get, gy(ky).get))
    }

    (cx, cy) match {
      case (Small(nx), Small(ny)) => finFin(nx, ny)
      case (Small(n), _)          => finInf(n, false)
      case (_, Small(n))          => finInf(n, true)
      case (_, _)                 => infInf
    }
  }

  /** Gets a list from an index. */
  def list[A](g: Get[A], c: Card): Get[List[A]] =
    new Get[List[A]] { gList =>
      private[this] val gCons = tuple2(g, c, gList, Inf)
      def apply(k: Nat): Option[List[A]] = {
        if (k == Nat.Zero) Some(Nil)
        else gCons(k - 1).map { case (x, xs) => x :: xs }
      }
    }

  /** Gets a set from an index. */
  def set[A](g: Get[A], c: Card): Get[Set[A]] = Get { k =>
    if (Small(k.bitSize) > c) None
    else Some(k.bits.zipWithIndex.collect { case (true, k) => g(Nat(k)).get }.toSet)
  }

  /** Gets a map from an index. */
  def map[A, B](gx: Get[A], cx: Fin, gy: Get[B], cy: Card): Get[Map[A, B]] = {
    val cListLeN = Card.sumOfGeometric(One, cy + 1, cx)
    val gCons = tuple2(listLeN(option(gy), cy + 1, cx - 1), cListLeN, gy, cy)
    Get { k =>
      if (k == Nat.Zero) Some(Map.empty)
      else
        gCons(k - 1).map { case (xys, y) =>
          val map1 = xys.zipWithIndex.collect { case (Some(x), k) => (gx(Nat(k)).get, x) }.toMap
          val map2 = Map(gx(Nat(xys.size)).get -> y)
          map1 ++ map2
        }
    }
  }

  /** Gets a function from an index. */
  def function1[A, B](gx: Get[A], cx: Fin, gy: Get[B], cy: Card): Get[A => B] =
    gy(Nat.Zero) match {
      case Some(y0) =>
        val gMap = map(gx, cx, Get(k => gy(k + 1)), cy - 1)
        Get(k => gMap(k).map(_.withDefaultValue(y0)))
      case None => Get(k => if (k == Nat.Zero) Some(Map.empty) else None)
    }

  /** Gets a partial function from an index. */
  def partialFunction[A, B](gx: Get[A], cx: Fin, gy: Get[B], cy: Card): Get[PartialFunction[A, B]] = {
    val gMap = map(gx, cx, gy, cy)
    Get { k => gMap(k) }
  }

  /** Gets an optional value from an index. */
  def option[A](g: Get[A]): Get[Option[A]] =
    Get(k => if (k == Nat.Zero) Some(None) else g(k - 1).map(Some(_)))

  /** Gets an either value from an index. */
  def either[A, B](gx: Get[A], cx: Card, gy: Get[B], cy: Card): Get[Either[A, B]] = {
    def smallLeft(nx: Nat): Get[Either[A, B]] = Get { k =>
      if (k >= nx * 2) gy(k - nx).map(Right(_))
      else {
        val (d, r) = k /% Nat.Two
        if (r == Nat.Zero) gx(d).map(Left(_)) else gy(d).map(Right(_))
      }
    }

    def smallRight(ny: Nat): Get[Either[A, B]] = Get { k =>
      if (k >= ny * 2) gx(k - ny).map(Left(_))
      else {
        val (d, r) = k /% Nat.Two
        if (r == Nat.Zero) gx(d).map(Left(_)) else gy(d).map(Right(_))
      }
    }

    def inf: Get[Either[A, B]] = Get { k =>
      val (d, r) = k /% Nat.Two
      if (r == Nat.Zero) gx(d).map(Left(_)) else gy(d).map(Right(_))
    }

    (cx, cy) match {
      case (Small(nx), Small(ny)) =>
        if (nx < ny) smallLeft(nx) else smallRight(ny)
      case (Small(nx), _) => smallLeft(nx)
      case (_, Small(ny)) => smallRight(ny)
      case (_, _)         => inf
    }
  }

  /** Computes a diagonal index from an index.
    *
    * See [[https://oeis.org/A003056 OEIS A003056]].
    */
  private def diagonal(n: Nat): Nat = (Nat.sqrt(n * 8 + 1) - 1) / 2

  /** Gets a list which sizes up to the given parameter from an index. */
  private def listLeN[A](g: Get[A], c: Card, size: Fin): Get[List[A]] =
    if (size.isZero) Get(k => if (k == Nat.Zero) Some(Nil) else None)
    else {
      val cListLeN = Card.sumOfGeometric(One, c, size)
      val gCons = tuple2(g, c, delay(listLeN(g, c, size - 1)), cListLeN)
      Get { k =>
        if (k == Nat.Zero) Some(Nil)
        else gCons(k - 1).map { case (x, xs) => x :: xs }
      }
    }
}
