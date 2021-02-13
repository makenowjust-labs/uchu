package codes.quine.labo.uchu

import codes.quine.labo.uchu.Cardinality._

class IndexOfSuite extends munit.FunSuite {
  val N = 200

  test("IndexOf.boolean") {
    assertEquals(IndexOf.boolean(false), 0: BigInt)
    assertEquals(IndexOf.boolean(true), 1: BigInt)
  }

  test("IndexOf.bigInt") {
    assertEquals(IndexOf.bigInt(0), 0: BigInt)
    assertEquals(IndexOf.bigInt(-1), 1: BigInt)
    assertEquals(IndexOf.bigInt(1), 2: BigInt)
  }

  test("IndexOf.byte") {
    assertEquals(IndexOf.byte(0), 0: BigInt)
    assertEquals(IndexOf.byte(-1), 1: BigInt)
    assertEquals(IndexOf.byte(1), 2: BigInt)
  }

  test("IndexOf.short") {
    assertEquals(IndexOf.short(0), 0: BigInt)
    assertEquals(IndexOf.short(-1), 1: BigInt)
    assertEquals(IndexOf.short(1), 2: BigInt)
  }

  test("IndexOf.int") {
    assertEquals(IndexOf.int(0), 0: BigInt)
    assertEquals(IndexOf.int(-1), 1: BigInt)
    assertEquals(IndexOf.int(1), 2: BigInt)
  }

  test("IndexOf.long") {
    assertEquals(IndexOf.long(0), 0: BigInt)
    assertEquals(IndexOf.long(-1), 1: BigInt)
    assertEquals(IndexOf.long(1), 2: BigInt)
  }

  test("IndexOf.tuple2: Fin == Fin") {
    val xs = Enumerate.tuple2(LazyList.range(0, 20), LazyList.range(0, 20))
    val indexOf = IndexOf.tuple2(BigInt(_: Int), Fin(20), BigInt(_: Int), Fin(20))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.tuple2: Fin < Fin") {
    val xs = Enumerate.tuple2(LazyList.range(0, 10), LazyList.range(0, 20))
    val indexOf = IndexOf.tuple2(BigInt(_: Int), Fin(10), BigInt(_: Int), Fin(20))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.tuple2: Fin > Fin") {
    val xs = Enumerate.tuple2(LazyList.range(0, 20), LazyList.range(0, 10))
    val indexOf = IndexOf.tuple2(BigInt(_: Int), Fin(20), BigInt(_: Int), Fin(10))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.tuple2: Fin < Inf") {
    val xs = Enumerate.tuple2(LazyList.range(0, 20), LazyList.iterate(0: BigInt)(_ + 1))
    val indexOf = IndexOf.tuple2(BigInt(_: Int), Fin(20), identity[BigInt], Inf)
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.tuple2: Inf > Fin") {
    val xs = Enumerate.tuple2(LazyList.iterate(0: BigInt)(_ + 1), LazyList.range(0, 20))
    val indexOf = IndexOf.tuple2(identity[BigInt], Inf, BigInt(_: Int), Fin(20))
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.tuple2: Inf == Inf") {
    val xs = Enumerate.tuple2(LazyList.iterate(0: BigInt)(_ + 1), LazyList.iterate(0: BigInt)(_ + 1))
    val indexOf = IndexOf.tuple2(identity[BigInt], Inf, identity[BigInt], Inf)
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.list: Fin") {
    val xs = Enumerate.list(LazyList.range(0, 20))
    val indexOf = IndexOf.list(BigInt(_: Int), Fin(20))
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.list: Inf") {
    val xs = Enumerate.list(LazyList.iterate(0: BigInt)(_ + 1))
    val indexOf = IndexOf.list(identity[BigInt], Inf)
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.set") {
    val xs = Enumerate.set(LazyList.range(0, 20), 20)
    val indexOf = IndexOf.set(BigInt(_: Int), Fin(20))
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("Index.map: Fin -> Fin") {
    val xs = Enumerate.map(LazyList.range(0, 20), 20, LazyList.range(0, 20))
    val indexOf = IndexOf.map(BigInt(_: Int), Fin(20), BigInt(_: Int), Fin(20))
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("Index.map: Fin -> Inf") {
    val xs = Enumerate.map(LazyList.range(0, 20), 20, LazyList.iterate(0: BigInt)(_ + 1))
    val indexOf = IndexOf.map(BigInt(_: Int), Fin(20), identity[BigInt], Inf)
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("Index.function1: Fin -> Fin") {
    val xs = Enumerate.function1(LazyList.range(0, 20), 20, LazyList.range(0, 20))
    val indexOf = IndexOf.function1(LazyList.range(0, 20), Fin(20), BigInt(_: Int), Fin(20))
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("Index.function1: Fin -> Inf") {
    val xs = Enumerate.function1(LazyList.range(0, 20), 20, LazyList.iterate(0: BigInt)(_ + 1))
    val indexOf = IndexOf.function1(LazyList.range(0, 20), Fin(20), identity[BigInt], Inf)
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("Index.partialFunction: Fin -> Fin") {
    val xs = Enumerate.map(LazyList.range(0, 20), 20, LazyList.range(0, 20))
    val indexOf = IndexOf.partialFunction(BigInt(_: Int), LazyList.range(0, 20), Fin(20), BigInt(_: Int), Fin(20))
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("Index.partialFunction: Fin -> Inf") {
    val xs = Enumerate.map(LazyList.range(0, 20), 20, LazyList.iterate(0: BigInt)(_ + 1))
    val indexOf = IndexOf.partialFunction(BigInt(_: Int), LazyList.range(0, 20), Fin(20), identity[BigInt], Inf)
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.option: Fin") {
    val xs = Enumerate.option(LazyList.range(0, 20))
    val indexOf = IndexOf.option(BigInt(_: Int))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.option: Inf") {
    val xs = Enumerate.option(LazyList.iterate(0: BigInt)(_ + 1))
    val indexOf = IndexOf.option(identity[BigInt])
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.either: Fin == Fin") {
    val xs = Enumerate.either(LazyList.range(0, 20), LazyList.range(0, 20))
    val indexOf = IndexOf.either(BigInt(_: Int), Fin(20), BigInt(_: Int), Fin(20))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.either: Fin < Fin") {
    val xs = Enumerate.either(LazyList.range(0, 10), LazyList.range(0, 20))
    val indexOf = IndexOf.either(BigInt(_: Int), Fin(10), BigInt(_: Int), Fin(20))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.either: Fin > Fin") {
    val xs = Enumerate.either(LazyList.range(0, 20), LazyList.range(0, 10))
    val indexOf = IndexOf.either(BigInt(_: Int), Fin(20), BigInt(_: Int), Fin(10))
    for ((x, i) <- xs.zipWithIndex) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.either: Fin < Inf") {
    val xs = Enumerate.either(LazyList.range(0, 20), LazyList.iterate(0: BigInt)(_ + 1))
    val indexOf = IndexOf.either(BigInt(_: Int), Fin(20), identity[BigInt], Inf)
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.either: Inf > Fin") {
    val xs = Enumerate.either(LazyList.iterate(0: BigInt)(_ + 1), LazyList.range(0, 20))
    val indexOf = IndexOf.either(identity[BigInt], Inf, BigInt(_: Int), Fin(20))
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }

  test("IndexOf.either: Inf == Inf") {
    val xs = Enumerate.either(LazyList.iterate(0: BigInt)(_ + 1), LazyList.iterate(0: BigInt)(_ + 1))
    val indexOf = IndexOf.either(identity[BigInt], Inf, identity[BigInt], Inf)
    for ((x, i) <- xs.zipWithIndex.take(N)) assertEquals(indexOf(x), BigInt(i))
  }
}
