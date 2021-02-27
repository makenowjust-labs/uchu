package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

class FiniteSuite extends munit.FunSuite {
  test("Finite.apply") {
    assertEquals(Finite[Boolean].enumerate, LazyList(false, true))
    assertEquals(Finite[Boolean].cardinality, Two)
  }

  test("Finite.of") {
    val iInt = IndexOf((n: Int) => Nat(n))
    val gInt = Get(k => Some(k.toInt))
    assertEquals(Finite.of(LazyList(0, 1), iInt, gInt).enumerate, LazyList(0, 1))
    assertEquals(Finite.of(LazyList(0, 1), iInt, gInt).cardinality, Two)
    assertEquals(Finite.of(LazyList(0, 1), iInt, gInt).indexOf(0), Nat.Zero)
    assertEquals(Finite.of(LazyList(0, 1), iInt, gInt).get(Nat.Zero), Some(0))
    assertEquals(Finite.of(LazyList(0, 1), Nat.Two, iInt, gInt).enumerate, LazyList(0, 1))
    assertEquals(Finite.of(LazyList(0, 1), Nat.Two, iInt, gInt).cardinality, Two)
    assertEquals(Finite.of(LazyList(0, 1), Nat.Two, iInt, gInt).indexOf(0), Nat.Zero)
    assertEquals(Finite.of(LazyList(0, 1), Nat.Two, iInt, gInt).get(Nat.Zero), Some(0))
    assertEquals(Finite.of(LazyList(0, 1), Two, iInt, gInt).enumerate, LazyList(0, 1))
    assertEquals(Finite.of(LazyList(0, 1), Two, iInt, gInt).cardinality, Two)
    assertEquals(Finite.of(LazyList(0, 1), Two, iInt, gInt).indexOf(0), Nat.Zero)
    assertEquals(Finite.of(LazyList(0, 1), Two, iInt, gInt).get(Nat.Zero), Some(0))
  }

  test("Finite.nothing") {
    assertEquals(Finite.nothing.enumerate, LazyList.empty)
    assertEquals(Finite.nothing.cardinality, Zero)
  }

  test("Finite.unit") {
    assertEquals(Finite.unit.enumerate, LazyList(()))
    assertEquals(Finite.unit.cardinality, One)
  }

  test("Finite.boolean") {
    assertEquals(Finite.boolean.enumerate, LazyList(false, true))
    assertEquals(Finite.boolean.cardinality, Two)
  }

  test("Finite.byte") {
    assertEquals(Finite.byte.enumerate.take(5), LazyList[Byte](0, -1, 1, -2, 2))
    assertEquals(Finite.byte.enumerate.size, 256)
    assertEquals(Finite.byte.cardinality, Small(256))
  }

  test("Finite.short") {
    assertEquals(Finite.short.enumerate.take(5), LazyList[Short](0, -1, 1, -2, 2))
    assertEquals(Finite.short.enumerate.size, 65536)
    assertEquals(Finite.short.cardinality, Small(65536))
  }

  test("Finite.int") {
    assertEquals(Finite.int.enumerate.take(5), LazyList(0, -1, 1, -2, 2))
    assertEquals(Finite.int.cardinality, Small(Nat.Two ** 32))
  }

  test("Finite.long") {
    assertEquals(Finite.long.enumerate.take(5), LazyList[Long](0, -1, 1, -2, 2))
    assertEquals(Finite.long.cardinality, Small(Nat.Two ** 64))
  }

  test("Finite.char") {
    assertEquals(Finite.char.enumerate.take(3), LazyList('\u0000', '\u0001', '\u0002'))
    assertEquals(Finite.char.cardinality, Small(65536))
  }

  test("Finite.tuple2") {
    assertEquals(
      Finite.tuple2(Finite[Byte], Finite[Byte]).enumerate.take(5),
      LazyList[(Byte, Byte)]((0, 0), (-1, 0), (0, -1), (1, 0), (-1, -1))
    )
    assertEquals(Finite.tuple2(Finite[Byte], Finite[Byte]).enumerate.size, 65536)
    assertEquals(Finite.tuple2(Finite[Byte], Finite[Byte]).cardinality, Small(65536))
  }

  test("Finite.option") {
    assertEquals(
      Finite.option(Finite[Byte]).enumerate.take(5),
      LazyList[Option[Byte]](None, Some(0), Some(-1), Some(1), Some(-2))
    )
    assertEquals(Finite.option(Finite[Byte]).enumerate.size, 257)
    assertEquals(Finite.option(Finite[Byte]).cardinality, Small(257))
  }

  test("Finite.either") {
    assertEquals(
      Finite.either(Finite[Byte], Finite[Byte]).enumerate.take(5),
      LazyList[Either[Byte, Byte]](Left(0), Right(0), Left(-1), Right(-1), Left(1))
    )
    assertEquals(Finite.either(Finite[Byte], Finite[Byte]).enumerate.size, 512)
    assertEquals(Finite.either(Finite[Byte], Finite[Byte]).cardinality, Small(512))
  }

  test("Finite.map") {
    assertEquals(
      Finite.map(Finite[Boolean], Finite[Boolean]).enumerate.take(5),
      LazyList[Map[Boolean, Boolean]](
        Map.empty,
        Map(false -> false),
        Map(true -> false),
        Map(false -> true),
        Map(false -> false, true -> false)
      )
    )
    assertEquals(Finite.map(Finite[Boolean], Finite[Boolean]).enumerate.size, 9)
    assertEquals(Finite.map(Finite[Boolean], Finite[Boolean]).cardinality, Small(9))
  }

  test("Finite.set") {
    assertEquals(
      Finite.set(Finite[Boolean]).enumerate,
      LazyList[Set[Boolean]](Set.empty, Set(false), Set(true), Set(false, true))
    )
    assertEquals(Finite.set(Finite[Boolean]).cardinality, Small(4))
  }

  test("Finite.function1") {
    assertEquals(
      Finite.function1(Finite[Boolean], Finite[Boolean]).enumerate,
      LazyList(
        Map.empty[Boolean, Boolean],
        Map(false -> true),
        Map(true -> true),
        Map(false -> true, true -> true)
      )
    )
    assertEquals(Finite.function1(Finite[Boolean], Finite[Boolean]).cardinality, Small(4))
  }

  test("Finite.partialFunction") {
    assertEquals(
      Finite.partialFunction(Finite[Boolean], Finite[Boolean]).enumerate.take(5),
      LazyList[Map[Boolean, Boolean]](
        Map.empty,
        Map(false -> false),
        Map(true -> false),
        Map(false -> true),
        Map(false -> false, true -> false)
      )
    )
    assertEquals(Finite.partialFunction(Finite[Boolean], Finite[Boolean]).enumerate.size, 9)
    assertEquals(Finite.partialFunction(Finite[Boolean], Finite[Boolean]).cardinality, Small(9))
  }
}
