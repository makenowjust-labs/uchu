package codes.quine.labo.uchu

class EnumSuite extends munit.FunSuite {
  test("Enum.boolean") {
    assertEquals(Enum.boolean, LazyList(false, true))
  }

  test("Enum.bigInt") {
    assertEquals(Enum.bigInt.take(5), LazyList[BigInt](0, -1, 1, -2, 2))
  }

  test("Enum.byte") {
    assertEquals(Enum.byte.take(5), LazyList[Byte](0, -1, 1, -2, 2))
  }

  test("Enum.short") {
    assertEquals(Enum.short.take(5), LazyList[Short](0, -1, 1, -2, 2))
  }

  test("Enum.int") {
    assertEquals(Enum.int.take(5), LazyList(0, -1, 1, -2, 2))
  }

  test("Enum.long") {
    assertEquals(Enum.long.take(5), LazyList[Long](0, -1, 1, -2, 2))
  }

  test("Enum.tuple2") {
    assertEquals(Enum.tuple2(LazyList(0, 1), LazyList(0, 1)), LazyList((0, 0), (1, 0), (0, 1), (1, 1)))
  }

  test("Enum.list") {
    assertEquals(Enum.list(LazyList(0, 1)).take(5), LazyList(List.empty, List(0), List(1), List(0, 0), List(1, 0)))
  }

  test("Enum.map") {
    assertEquals(
      Enum.map(LazyList(0, 1), 2, LazyList(0, 1)),
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

  test("Enum.set") {
    assertEquals(
      Enum.set(LazyList(0, 1), 2),
      LazyList(
        Set.empty[Int],
        Set(0),
        Set(1),
        Set(0, 1)
      )
    )
  }

  test("Enum.function2") {
    assertEquals(
      Enum.function2(LazyList(0, 1), 2, LazyList(0, 1)),
      LazyList(
        Map(0 -> 0, 1 -> 0),
        Map(0 -> 1, 1 -> 0),
        Map(0 -> 0, 1 -> 1),
        Map(0 -> 1, 1 -> 1)
      )
    )
  }

  test("Enum.option") {
    assertEquals(Enum.option(LazyList(0, 1)), LazyList(None, Some(0), Some(1)))
  }

  test("Enum.either") {
    assertEquals(Enum.either(LazyList(0, 1), LazyList(0, 1)), LazyList(Left(0), Right(0), Left(1), Right(1)))
  }

  test("Enum.diagonal") {
    assertEquals(
      Enum.diagonal(LazyList(LazyList((0, 0), (0, 1)), LazyList((1, 0), (1, 1)))),
      LazyList((0, 0), (1, 0), (0, 1), (1, 1))
    )
  }

  test("Enum.sized") {
    assertEquals(
      Enum.sized(LazyList(0, 1), 2),
      LazyList(List.empty, List(0), List(1), List(0, 0), List(1, 0), List(0, 1), List(1, 1))
    )
  }

  test("Enum.sized") {
    assertEquals(
      Enum.listN(LazyList(0, 1), 2),
      LazyList(List(0, 0), List(1, 0), List(0, 1), List(1, 1))
    )
  }

  test("Enum.interleave") {
    assertEquals(Enum.interleave(LazyList(0, 1), LazyList(2, 3)), LazyList(0, 2, 1, 3))
  }
}
