package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

/** Enum is utilities for enumerating possible values.
  *
  * The methods in this are used for implementing [[Universe.enumerate]].
  */
object Enumerate {

  /** Delays building a lazy list. */
  def delay[A](xs: => LazyList[A]): LazyList[A] =
    LazyList.cons(xs, LazyList.empty).flatten

  /** Enumerates possible [[Boolean]] values. */
  def boolean: LazyList[Boolean] = LazyList(false, true)

  /** Enumerates possible [[BigInt]] values. */
  def bigInt: LazyList[BigInt] =
    LazyList.cons(0, LazyList.iterate(1: BigInt)(_ + 1).flatMap(n => Seq(-n, n)))

  /** Enumerates possible [[Byte]] values. */
  def byte: LazyList[Byte] =
    bigInt.takeWhile(n => n >= Byte.MinValue && n <= Byte.MaxValue).map(_.toByte)

  /** Enumerates possible [[Short]] values. */
  def short: LazyList[Short] =
    bigInt.takeWhile(n => n >= Short.MinValue && n <= Short.MaxValue).map(_.toShort)

  /** Enumerates possible [[Int]] values. */
  def int: LazyList[Int] =
    bigInt.takeWhile(n => n >= Int.MinValue && n <= Int.MaxValue).map(_.toInt)

  /** Enumerates possible [[Long]] values. */
  def long: LazyList[Long] =
    bigInt.takeWhile(n => n >= Long.MinValue && n <= Long.MaxValue).map(_.toLong)

  /** Enumerates possible [[Char]] values. */
  def char: LazyList[Char] = LazyList.range(0, 65536).map(_.toChar)

  /** Enumerates possible [[String]] values. */
  def string: LazyList[String] = seq(char).map(_.mkString)

  /** Enumerates a pair of values in diagonal order. */
  def tuple2[A, B](xs: LazyList[A], ys: LazyList[B]): LazyList[(A, B)] =
    diagonal(xs.map(x => ys.map((x, _))))

  /** Enumerates possible lists in diagonal order. */
  def seq[A](xs: LazyList[A]): LazyList[Seq[A]] =
    LazyList.cons(Nil, tuple2(xs, seq(xs)).map { case (x, xs) => x +: xs })

  /** Enumerates possible maps in diagonal order. */
  def map[A, B](xs: LazyList[A], cx: Fin, ys: LazyList[B]): LazyList[Map[A, B]] =
    LazyList.cons(
      Map.empty,
      if (cx.isZero) LazyList.empty
      else {
        val values = tuple2(listLeN(option(ys), cx - 1), ys).map { case (ys, y) => ys :+ Some(y) }
        values.map(_.zip(xs).collect { case (Some(y), x) => (x, y) }.toMap)
      }
    )

  /** Enumerates possible non-empty maps in diagonal order. */
  def nonEmptyMap[A, B](xs: LazyList[A], cx: Fin, ys: LazyList[B]): LazyList[Map[A, B]] = map(xs, cx, ys).drop(1)

  /** Enumerates possible sets in diagonal order. */
  def set[A](xs: LazyList[A], c: Fin): LazyList[Set[A]] =
    natural.takeWhile(n => Small(n.bitSize) <= c).map(_.bits.zip(xs).collect { case (true, v) => v }.toSet)

  /** Enumerates possible non-empty maps in diagonaal order. */
  def nonEmptySet[A](xs: LazyList[A], c: Fin): LazyList[Set[A]] = set(xs, c).drop(1)

  /** Enumerates possible functions in diagonal order. */
  def function1[A, B](ix: IndexOf[A], cx: Fin, ys: LazyList[B]): LazyList[A => B] =
    ys.headOption match {
      case Some(y0) => map(to(cx), cx, ys.tail).map(MapFunction(ix, _, y0))
      case None     => LazyList(_ => throw new NoSuchElementException)
    }

  /** Enumerates possible optional values. */
  def option[A](xs: LazyList[A]): LazyList[Option[A]] =
    LazyList.cons(None, xs.map(Some(_)))

  /** Enumerates possible either values. */
  def either[A, B](xs: LazyList[A], ys: LazyList[B]): LazyList[Either[A, B]] =
    interleave(xs.map(Left(_)), ys.map(Right(_)))

  /** Enumerates natural numbers. */
  private def natural: LazyList[Nat] = LazyList.iterate(Nat.Zero)(_ + 1)

  /** Enumerates table values in diagonal order. */
  private def diagonal[A](table: LazyList[LazyList[A]]): LazyList[A] = {
    def diagonals(edges: Seq[LazyList[A]], table: LazyList[LazyList[A]]): LazyList[Seq[A]] =
      LazyList.cons(
        edges.flatMap(_.headOption), {
          val nextEdges = edges.map(_.tail).filter(_.nonEmpty)
          table.headOption match {
            case Some(edge) => diagonals(edge +: nextEdges, table.tail)
            case None       => transpose(nextEdges)
          }
        }
      )

    def transpose(edges: Seq[LazyList[A]]): LazyList[Seq[A]] =
      if (edges.isEmpty) LazyList.empty
      else LazyList.cons(edges.flatMap(_.headOption), transpose(edges.map(_.tail).filter(_.nonEmpty)))

    diagonals(Seq.empty, table).flatten
  }

  /** Counts a number of the list elements. */
  private[uchu] def sizeOf[A](xs: Seq[A]): Nat = xs.foldLeft(Nat.Zero)((n, _) => n + 1)

  /** Creates a list contains natural numbers from 0 to `n` sequentially. */
  private[uchu] def to(n: Nat): LazyList[Nat] = to(Small(n))

  /** Creates a list contains natural numbers from 0 to `n` sequentially. */
  private[uchu] def to(n: Fin): LazyList[Nat] =
    LazyList.unfold(Nat.Zero)(k => if (Small(k) >= n) None else Some((k, k + 1)))

  /** Enumerates possible lists which size up to the given parameter in diagonal order. */
  private def listLeN[A](xs: LazyList[A], size: Fin): LazyList[List[A]] =
    if (size.isZero) LazyList(Nil)
    else LazyList.cons(Nil, tuple2(xs, listLeN(xs, size - 1)).map { case (x, xs) => x :: xs })

  /** Enumerates two values with interleaving. */
  private def interleave[A](xs: LazyList[A], ys: LazyList[A]): LazyList[A] =
    xs.headOption match {
      case Some(x) => LazyList.cons(x, interleave(ys, xs.tail))
      case None    => ys
    }
}
