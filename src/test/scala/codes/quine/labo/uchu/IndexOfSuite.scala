package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

class IndexOfSuite extends munit.FunSuite {
  test("IndexOf.unit") {
    assertEquals(IndexOf.unit(()), N.Zero)
  }

  test("IndexOf.boolean") {
    assertEquals(IndexOf.boolean(false), N.Zero)
    assertEquals(IndexOf.boolean(true), N.One)
  }

  test("IndexOf.bigInt") {
    assertEquals(IndexOf.bigInt(0), N.Zero)
    assertEquals(IndexOf.bigInt(-1), N.One)
    assertEquals(IndexOf.bigInt(1), N.Two)
  }

  test("IndexOf.byte") {
    assertEquals(IndexOf.byte(0), N.Zero)
    assertEquals(IndexOf.byte(-1), N.One)
    assertEquals(IndexOf.byte(1), N.Two)
  }

  test("IndexOf.short") {
    assertEquals(IndexOf.short(0), N.Zero)
    assertEquals(IndexOf.short(-1), N.One)
    assertEquals(IndexOf.short(1), N.Two)
  }

  test("IndexOf.int") {
    assertEquals(IndexOf.int(0), N.Zero)
    assertEquals(IndexOf.int(-1), N.One)
    assertEquals(IndexOf.int(1), N.Two)
  }

  test("IndexOf.long") {
    assertEquals(IndexOf.long(0), N.Zero)
    assertEquals(IndexOf.long(-1), N.One)
    assertEquals(IndexOf.long(1), N.Two)
  }

  val size = 200
  val iInt: IndexOf[Int] = IndexOf((x: Int) => N(x))
  val iBigInt: IndexOf[BigInt] = IndexOf((x: BigInt) => N(x))
  val xs10: LazyList[Int] = LazyList.range(0, 10)
  val xs20: LazyList[Int] = LazyList.range(0, 20)
  val xsInf: LazyList[BigInt] = LazyList.iterate(0: BigInt)(_ + 1)

  test("IndexOf.tuple2: Fin == Fin") {
    val xs = Enumerate.tuple2(xs20, xs20)
    val indexOf = IndexOf.tuple2(iInt, Small(20), iInt, Small(20))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.tuple2: Fin < Fin") {
    val xs = Enumerate.tuple2(xs10, xs20)
    val indexOf = IndexOf.tuple2(iInt, Small(10), iInt, Small(20))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.tuple2: Fin > Fin") {
    val xs = Enumerate.tuple2(xs20, xs10)
    val indexOf = IndexOf.tuple2(iInt, Small(20), iInt, Small(10))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.tuple2: Fin < Inf") {
    val xs = Enumerate.tuple2(xs20, xsInf)
    val indexOf = IndexOf.tuple2(iInt, Small(20), iBigInt, Inf)
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.tuple2: Inf > Fin") {
    val xs = Enumerate.tuple2(xsInf, xs20)
    val indexOf = IndexOf.tuple2(iBigInt, Inf, iInt, Small(20))
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.tuple2: Inf == Inf") {
    val xs = Enumerate.tuple2(xsInf, xsInf)
    val indexOf = IndexOf.tuple2(iBigInt, Inf, iBigInt, Inf)
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.list: Fin") {
    val xs = Enumerate.list(xs20)
    val indexOf = IndexOf.list(iInt, Small(20))
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.list: Inf") {
    val xs = Enumerate.list(xsInf)
    val indexOf = IndexOf.list(iBigInt, Inf)
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.set") {
    val xs = Enumerate.set(xs10, Small(10))
    val indexOf = IndexOf.set(iInt)
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), N(i))
  }

  test("Index.map: Fin -> Fin (small)") {
    val xs = Enumerate.map(Enumerate.boolean, Small(2), Enumerate.boolean)
    val indexOf = IndexOf.map(IndexOf.boolean, Small(2), IndexOf.boolean, Small(2))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), N(i))
  }

  test("Index.map: Fin -> Fin (large)") {
    val xs = Enumerate.map(xs20, Small(20), xs20)
    val indexOf = IndexOf.map(iInt, Small(20), iInt, Small(20))
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("Index.map: Fin -> Inf") {
    val xs = Enumerate.map(xs20, Small(20), xsInf)
    val indexOf = IndexOf.map(iInt, Small(20), iBigInt, Inf)
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("Index.function1: Fin -> Fin (small)") {
    val xs = Enumerate.function1(Enumerate.boolean, Small(2), Enumerate.boolean)
    val indexOf = IndexOf.function1(Enumerate.boolean, IndexOf.boolean, Small(2), IndexOf.boolean, Small(2))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), N(i))
  }

  test("Index.function1: Fin -> Fin (large)") {
    val xs = Enumerate.function1(xs20, Small(20), xs20)
    val indexOf = IndexOf.function1(xs20, iInt, Small(20), iInt, Small(20))
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("Index.function1: Fin -> Inf") {
    val xs = Enumerate.function1(xs20, Small(20), xsInf)
    val indexOf = IndexOf.function1(xs20, iInt, Small(20), iBigInt, Inf)
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("Index.partialFunction: Fin -> Fin") {
    val xs = Enumerate.map(xs20, Small(20), xs20)
    val indexOf = IndexOf.partialFunction(xs20, iInt, Small(20), iInt, Small(20))
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("Index.partialFunction: Fin -> Inf") {
    val xs = Enumerate.map(xs20, Small(20), xsInf)
    val indexOf = IndexOf.partialFunction(xs20, iInt, Small(20), iBigInt, Inf)
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.option: Fin") {
    val xs = Enumerate.option(xs20)
    val indexOf = IndexOf.option(iInt)
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.option: Inf") {
    val xs = Enumerate.option(xsInf)
    val indexOf = IndexOf.option(iBigInt)
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.either: Fin == Fin") {
    val xs = Enumerate.either(xs20, xs20)
    val indexOf = IndexOf.either(iInt, Small(20), iInt, Small(20))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.either: Fin < Fin") {
    val xs = Enumerate.either(xs10, xs20)
    val indexOf = IndexOf.either(iInt, Small(10), iInt, Small(20))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.either: Fin > Fin") {
    val xs = Enumerate.either(xs20, xs10)
    val indexOf = IndexOf.either(iInt, Small(20), iInt, Small(10))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.either: Fin < Inf") {
    val xs = Enumerate.either(xs20, xsInf)
    val indexOf = IndexOf.either(iInt, Small(20), iBigInt, Inf)
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.either: Inf > Fin") {
    val xs = Enumerate.either(xsInf, xs20)
    val indexOf = IndexOf.either(iBigInt, Inf, iInt, Small(20))
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }

  test("IndexOf.either: Inf == Inf") {
    val xs = Enumerate.either(xsInf, xsInf)
    val indexOf = IndexOf.either(iBigInt, Inf, iBigInt, Inf)
    for ((x, i) <- xs.zipWithIndex.take(size)) assertEquals(indexOf(x), N(i))
  }
}
