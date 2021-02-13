package codes.quine.labo.uchu

/** Enum is utilities for enumerating possible values.
  *
  * The methods in this are used for implementing [[Universe.universe]].
  */
private[uchu] object Enum {

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

  /** Enumerates pair values in diagonal order. */
  def tuple2[A, B](xs: LazyList[A], ys: LazyList[B]): LazyList[(A, B)] =
    diagonal(xs.map(x => ys.map((x, _))))

  /** Enumerates possible lists in diagonal order. */
  def list[A](xs: LazyList[A]): LazyList[List[A]] =
    LazyList.cons(Nil, tuple2(xs, list(xs)).map { case (x, xs) => x :: xs })

  /** Enumerates possible maps in diagonal order. */
  def map[A, B](xs: LazyList[A], xsSize: BigInt, ys: LazyList[B]): LazyList[Map[A, B]] =
    LazyList.cons(
      Map.empty,
      if (xsSize > 0) {
        val values = tuple2(sized(option(ys), xsSize - 1), ys).map { case (ys, y) => ys :+ Some(y) }
        values.map(_.zip(xs).collect { case (Some(y), x) => (x, y) }.toMap)
      } else LazyList.empty
    )

  /** Enumerates possible sets in diagonal order. */
  def set[A](xs: LazyList[A], xsSize: BigInt): LazyList[Set[A]] =
    LazyList.cons(
      Set.empty,
      if (xsSize > 0) {
        val values = Enum.sized(boolean, xsSize - 1).map(_ :+ true)
        values.map(_.zip(xs).collect { case (true, x) => x }.toSet)
      } else LazyList.empty
    )

  /** Enumerates possible functions in diagonal order. */
  def function2[A, B](xs: LazyList[A], xsSize: BigInt, ys: LazyList[B]): LazyList[A => B] =
    listN(ys, xsSize).map(_.zip(xs).map { case (y, x) => (x, y) }.toMap)

  /** Enumerates possible optional values. */
  def option[A](xs: LazyList[A]): LazyList[Option[A]] =
    LazyList.cons(None, xs.map(Some(_)))

  /** Enumerates possible either values. */
  def either[A, B](xs: LazyList[A], ys: LazyList[B]): LazyList[Either[A, B]] =
    interleave(xs.map(Left(_)), ys.map(Right(_)))

  /** Enumerates table values in diagonal order. */
  def diagonal[A](table: LazyList[LazyList[A]]): LazyList[A] = {
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

  /** Enumerates possible lists which size up to the given parameter in diagonal order. */
  def sized[A](xs: LazyList[A], size: BigInt): LazyList[List[A]] =
    if (size <= 0) LazyList(Nil)
    else LazyList.cons(Nil, tuple2(xs, sized(xs, size - 1)).map { case (x, xs) => x :: xs })

  /** Enumerates possible list which sze to the given parameter in diagonal order. */
  def listN[A](xs: LazyList[A], size: BigInt): LazyList[List[A]] =
    if (size <= 0) LazyList(Nil)
    else tuple2(xs, listN(xs, size - 1)).map { case (x, xs) => x :: xs }

  /** Enumerates two values with interleaving. */
  def interleave[A](xs: LazyList[A], ys: LazyList[A]): LazyList[A] =
    xs.headOption match {
      case Some(x) => LazyList.cons(x, interleave(ys, xs.tail))
      case None    => ys
    }
}
