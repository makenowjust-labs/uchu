package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

class GetSuite extends munit.FunSuite {
  test("Get.nothing") {
    assertEquals(Get.nothing(N.Zero), None)
  }

  test("Get.boolean") {
    assertEquals(Get.boolean(N.Zero), Some(false))
    assertEquals(Get.boolean(N.One), Some(true))
    assertEquals(Get.boolean(N.Two), None)
  }

  test("Get.bigInt") {
    assertEquals(Get.bigInt(N.Zero), Some(0: BigInt))
    assertEquals(Get.bigInt(N.One), Some(-1: BigInt))
    assertEquals(Get.bigInt(N.Two), Some(1: BigInt))
  }

  test("Get.byte") {
    assertEquals(Get.byte(N.Zero), Some(0: Byte))
    assertEquals(Get.byte(N.One), Some(-1: Byte))
    assertEquals(Get.byte(N.Two), Some(1: Byte))
    assertEquals(Get.byte(N.Two ** 8), None)
  }

  test("Get.short") {
    assertEquals(Get.short(N.Zero), Some(0: Short))
    assertEquals(Get.short(N.One), Some(-1: Short))
    assertEquals(Get.short(N.Two), Some(1: Short))
    assertEquals(Get.short(N.Two ** 16), None)
  }

  test("Get.int") {
    assertEquals(Get.int(N.Zero), Some(0))
    assertEquals(Get.int(N.One), Some(-1))
    assertEquals(Get.int(N.Two), Some(1))
    assertEquals(Get.int(N.Two ** 64), None)
  }

  test("Get.long") {
    assertEquals(Get.long(N.Zero), Some(0: Long))
    assertEquals(Get.long(N.One), Some(-1: Long))
    assertEquals(Get.long(N.Two), Some(1: Long))
    assertEquals(Get.long(N.Two ** 64), None)
  }

  val size = 200
  val gInt: Get[Int] = Get(k => Some(k.toInt))
  val gBigInt: Get[BigInt] = Get(k => Some(k.value))
  val xs10: LazyList[Int] = LazyList.range(0, 10)
  val xs20: LazyList[Int] = LazyList.range(0, 20)
  val xsInf: LazyList[BigInt] = LazyList.iterate(0: BigInt)(_ + 1)

  test("Get.tuple2: Fin == Fin") {
    val xs = Enumerate.tuple2(xs20, xs20)
    val g = Get.tuple2(gInt, Small(20), gInt, Small(20))
    for ((x, k) <- xs.zipWithIndex) assertEquals(g(N(k)), Some(x))
  }

  test("Get.tuple2: Fin < Fin") {
    val xs = Enumerate.tuple2(xs10, xs20)
    val g = Get.tuple2(gInt, Small(10), gInt, Small(20))
    for ((x, k) <- xs.zipWithIndex) assertEquals(g(N(k)), Some(x))
  }

  test("Get.tuple2: Fin > Fin") {
    val xs = Enumerate.tuple2(xs20, xs10)
    val g = Get.tuple2(gInt, Small(20), gInt, Small(10))
    for ((x, k) <- xs.zipWithIndex) assertEquals(g(N(k)), Some(x))
  }

  test("Get.tuple2: Fin < Inf") {
    val xs = Enumerate.tuple2(xs20, xsInf)
    val g = Get.tuple2(gInt, Small(20), gBigInt, Inf)
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.tuple2: Inf > Fin") {
    val xs = Enumerate.tuple2(xsInf, xs20)
    val g = Get.tuple2(gBigInt, Inf, gInt, Small(20))
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.tuple2: Inf == Inf") {
    val xs = Enumerate.tuple2(xsInf, xsInf)
    val g = Get.tuple2(gBigInt, Inf, gBigInt, Inf)
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.list: Fin") {
    val xs = Enumerate.list(xs20)
    val g = Get.list(gInt, Small(20))
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.list: Inf") {
    val xs = Enumerate.list(xsInf)
    val g = Get.list(gBigInt, Inf)
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.set") {
    val xs = Enumerate.set(xs20, Small(20))
    val g = Get.set(gInt, Small(20))
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.map: Fin -> Fin") {
    val xs = Enumerate.map(xs20, Small(20), xs20)
    val g = Get.map(gInt, Small(20), gInt, Small(20))
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.map: Fin -> Inf") {
    val xs = Enumerate.map(xs20, Small(20), xsInf)
    val g = Get.map(gInt, Small(20), gBigInt, Inf)
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.function1: Fin -> Fin") {
    val xs = Enumerate.function1(xs20, Small(20), xs20)
    val g = Get.function1(xs20, Small(20), gInt, Small(20))
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.function1: Fin -> Inf") {
    val xs = Enumerate.function1(xs20, Small(20), xsInf)
    val g = Get.function1(xs20, Small(20), gBigInt, Inf)
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.partialFunction: Fin -> Fin") {
    val xs = Enumerate.function1(xs20, Small(20), xs20)
    val g = Get.function1(xs20, Small(20), gInt, Small(20))
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.partialFunction: Fin -> Inf") {
    val xs = Enumerate.function1(xs20, Small(20), xsInf)
    val g = Get.function1(xs20, Small(20), gBigInt, Inf)
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.option: Fin") {
    val xs = Enumerate.option(xs20)
    val g = Get.option(gInt)
    for ((x, k) <- xs.zipWithIndex) assertEquals(g(N(k)), Some(x))
  }

  test("Get.option: Inf") {
    val xs = Enumerate.option(xsInf)
    val g = Get.option(gBigInt)
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.either: Fin == Fin") {
    val xs = Enumerate.either(xs20, xs20)
    val g = Get.either(gInt, Small(20), gInt, Small(20))
    for ((x, k) <- xs.zipWithIndex) assertEquals(g(N(k)), Some(x))
  }

  test("Get.either: Fin < Fin") {
    val xs = Enumerate.either(xs10, xs20)
    val g = Get.either(gInt, Small(10), gInt, Small(20))
    for ((x, k) <- xs.zipWithIndex) assertEquals(g(N(k)), Some(x))
  }

  test("Get.either: Fin > Fin") {
    val xs = Enumerate.either(xs20, xs10)
    val g = Get.either(gInt, Small(20), gInt, Small(10))
    for ((x, k) <- xs.zipWithIndex) assertEquals(g(N(k)), Some(x))
  }

  test("Get.either: Fin < Inf") {
    val xs = Enumerate.either(xs20, xsInf)
    val g = Get.either(gInt, Small(20), gBigInt, Inf)
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.either: Inf > Fin") {
    val xs = Enumerate.either(xsInf, xs20)
    val g = Get.either(gBigInt, Inf, gInt, Small(20))
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }

  test("Get.either: Inf == Inf") {
    val xs = Enumerate.either(xsInf, xsInf)
    val g = Get.either(gBigInt, Inf, gBigInt, Inf)
    for ((x, k) <- xs.zipWithIndex.take(size)) assertEquals(g(N(k)), Some(x))
  }
}
