package codes.quine.labo.uchu

import Cardinality.Fin

class FiniteSuite extends munit.FunSuite {
  test("Finite.apply") {
    assertEquals(Finite[Boolean].universe, LazyList(false, true))
    assertEquals(Finite[Boolean].size, 2: BigInt)
  }

  test("Finite.of") {
    assertEquals(Finite.of(LazyList(0, 1)).universe, LazyList(0, 1))
    assertEquals(Finite.of(LazyList(0, 1)).size, 2: BigInt)
    assertEquals(Finite.of(LazyList(0, 1), 2).universe, LazyList(0, 1))
    assertEquals(Finite.of(LazyList(0, 1), 2).size, 2: BigInt)
    assertEquals(Finite.of(LazyList(0, 1), Fin(2)).universe, LazyList(0, 1))
    assertEquals(Finite.of(LazyList(0, 1), Fin(2)).size, 2: BigInt)
  }

  test("Finite.nothing") {
    assertEquals(Finite.nothing.universe, LazyList.empty)
    assertEquals(Finite.nothing.size, 0: BigInt)
  }

  test("Finite.unit") {
    assertEquals(Finite.unit.universe, LazyList(()))
    assertEquals(Finite.unit.size, 1: BigInt)
  }

  test("Finite.boolean") {
    assertEquals(Finite.boolean.universe, LazyList(false, true))
    assertEquals(Finite.boolean.size, 2: BigInt)
  }

  test("Finite.byte") {
    assertEquals(Finite.byte.universe.take(5), LazyList[Byte](0, -1, 1, -2, 2))
    assertEquals(Finite.byte.universe.size, 256)
    assertEquals(Finite.byte.size, 256: BigInt)
  }

  test("Finite.short") {
    assertEquals(Finite.short.universe.take(5), LazyList[Short](0, -1, 1, -2, 2))
    assertEquals(Finite.short.universe.size, 65536)
    assertEquals(Finite.short.size, 65536: BigInt)
  }

  test("Finite.int") {
    assertEquals(Finite.int.universe.take(5), LazyList(0, -1, 1, -2, 2))
    assertEquals(Finite.int.size, (2: BigInt).pow(32))
  }

  test("Finite.long") {
    assertEquals(Finite.long.universe.take(5), LazyList[Long](0, -1, 1, -2, 2))
    assertEquals(Finite.long.size, (2: BigInt).pow(64))
  }

  test("Finite.tuple2") {
    assertEquals(
      Finite.tuple2(Finite[Byte], Finite[Byte]).universe.take(5),
      LazyList[(Byte, Byte)]((0, 0), (-1, 0), (0, -1), (1, 0), (-1, -1))
    )
    assertEquals(Finite.tuple2(Finite[Byte], Finite[Byte]).universe.size, 65536)
    assertEquals(Finite.tuple2(Finite[Byte], Finite[Byte]).size, 65536: BigInt)
  }

  test("Finite.option") {
    assertEquals(
      Finite.option(Finite[Byte]).universe.take(5),
      LazyList[Option[Byte]](None, Some(0), Some(-1), Some(1), Some(-2))
    )
    assertEquals(Finite.option(Finite[Byte]).universe.size, 257)
    assertEquals(Finite.option(Finite[Byte]).size, 257: BigInt)
  }

  test("Finite.either") {
    assertEquals(
      Finite.either(Finite[Byte], Finite[Byte]).universe.take(5),
      LazyList[Either[Byte, Byte]](Left(0), Right(0), Left(-1), Right(-1), Left(1))
    )
    assertEquals(Finite.either(Finite[Byte], Finite[Byte]).universe.size, 512)
    assertEquals(Finite.either(Finite[Byte], Finite[Byte]).size, 512: BigInt)
  }

  test("Finite.map") {
    assertEquals(
      Finite.map(Finite[Boolean], Finite[Boolean]).universe.take(5),
      LazyList[Map[Boolean, Boolean]](
        Map.empty,
        Map(false -> false),
        Map(true -> false),
        Map(false -> true),
        Map(false -> false, true -> false)
      )
    )
    assertEquals(Finite.map(Finite[Boolean], Finite[Boolean]).universe.size, 9)
    assertEquals(Finite.map(Finite[Boolean], Finite[Boolean]).size, 9: BigInt)
  }

  test("Finite.set") {
    assertEquals(
      Finite.set(Finite[Boolean]).universe,
      LazyList[Set[Boolean]](Set.empty, Set(false), Set(true), Set(false, true))
    )
    assertEquals(Finite.set(Finite[Boolean]).size, 4: BigInt)
  }

  test("Finite.function2") {
    assertEquals(
      Finite.function2(Finite[Boolean], Finite[Boolean]).universe,
      LazyList(
        Map(false -> false, true -> false),
        Map(false -> true, true -> false),
        Map(false -> false, true -> true),
        Map(false -> true, true -> true)
      )
    )
    assertEquals(Finite.function2(Finite[Boolean], Finite[Boolean]).size, 4: BigInt)
  }

  test("Finite.partialFunction") {
    assertEquals(
      Finite.partialFunction(Finite[Boolean], Finite[Boolean]).universe.take(5),
      LazyList[Map[Boolean, Boolean]](
        Map.empty,
        Map(false -> false),
        Map(true -> false),
        Map(false -> true),
        Map(false -> false, true -> false)
      )
    )
    assertEquals(Finite.partialFunction(Finite[Boolean], Finite[Boolean]).universe.size, 9)
    assertEquals(Finite.partialFunction(Finite[Boolean], Finite[Boolean]).size, 9: BigInt)

  }
}
