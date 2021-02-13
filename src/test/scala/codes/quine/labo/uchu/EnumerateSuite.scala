package codes.quine.labo.uchu

class EnumerateSuite extends munit.FunSuite {
  test("Enumerate.boolean") {
    assertEquals(Enumerate.boolean, LazyList(false, true))
  }

  test("Enumerate.bigInt") {
    assertEquals(Enumerate.bigInt.take(5), LazyList[BigInt](0, -1, 1, -2, 2))
  }

  test("Enumerate.byte") {
    assertEquals(Enumerate.byte.take(5), LazyList[Byte](0, -1, 1, -2, 2))
  }

  test("Enumerate.short") {
    assertEquals(Enumerate.short.take(5), LazyList[Short](0, -1, 1, -2, 2))
  }

  test("Enumerate.int") {
    assertEquals(Enumerate.int.take(5), LazyList(0, -1, 1, -2, 2))
  }

  test("Enumerate.long") {
    assertEquals(Enumerate.long.take(5), LazyList[Long](0, -1, 1, -2, 2))
  }

  test("Enumerate.tuple2") {
    assertEquals(Enumerate.tuple2(LazyList(0, 1), LazyList(0, 1)), LazyList((0, 0), (1, 0), (0, 1), (1, 1)))
  }

  test("Enumerate.list") {
    assertEquals(Enumerate.list(LazyList(0, 1)).take(5), LazyList(List.empty, List(0), List(1), List(0, 0), List(1, 0)))
  }

  test("Enumerate.map") {
    assertEquals(Enumerate.map(LazyList.empty, 0, LazyList(0, 1)), LazyList(Map.empty))
    assertEquals(
      Enumerate.map(LazyList(0, 1), 2, LazyList(0, 1)),
      LazyList(
        Map.empty[Int, Int],
        Map(0 -> 0),
        Map(1 -> 0),
        Map(0 -> 1),
        Map(0 -> 0, 1 -> 0),
        Map(1 -> 1),
        Map(0 -> 1, 1 -> 0),
        Map(0 -> 0, 1 -> 1),
        Map(0 -> 1, 1 -> 1)
      )
    )
  }

  test("Enumerate.set") {
    assertEquals(Enumerate.set(LazyList.empty, 0), LazyList(Set.empty))
    assertEquals(
      Enumerate.set(LazyList(0, 1), 2),
      LazyList(
        Set.empty[Int],
        Set(0),
        Set(1),
        Set(0, 1)
      )
    )
  }

  test("Enumerate.function1") {
    assertEquals(
      Enumerate.function1(LazyList(0, 1), 2, LazyList(0, 1)),
      LazyList(
        Map(0 -> 0, 1 -> 0),
        Map(0 -> 1, 1 -> 0),
        Map(0 -> 0, 1 -> 1),
        Map(0 -> 1, 1 -> 1)
      )
    )
  }

  test("Enumerate.option") {
    assertEquals(Enumerate.option(LazyList(0, 1)), LazyList(None, Some(0), Some(1)))
  }

  test("Enumerate.either") {
    assertEquals(Enumerate.either(LazyList(0, 1), LazyList(0, 1)), LazyList(Left(0), Right(0), Left(1), Right(1)))
  }

  test("Enumerate.diagonal") {
    assertEquals(
      Enumerate.diagonal(LazyList(LazyList((0, 0), (0, 1)), LazyList((1, 0), (1, 1)))),
      LazyList((0, 0), (1, 0), (0, 1), (1, 1))
    )
  }

  test("Enumerate.sized") {
    assertEquals(
      Enumerate.sized(LazyList(0, 1), 2),
      LazyList(List.empty, List(0), List(1), List(0, 0), List(1, 0), List(0, 1), List(1, 1))
    )
  }

  test("Enumerate.listN") {
    assertEquals(
      Enumerate.listN(LazyList(0, 1), 2),
      LazyList(List(0, 0), List(1, 0), List(0, 1), List(1, 1))
    )
  }

  test("Enumerate.interleave") {
    assertEquals(Enumerate.interleave(LazyList(0, 1), LazyList(2, 3)), LazyList(0, 2, 1, 3))
  }
}
