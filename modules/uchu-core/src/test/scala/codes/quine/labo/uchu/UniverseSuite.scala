package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

class UniverseSuite extends munit.FunSuite {
  test("Universe.apply") {
    assertEquals(Universe[Boolean].enumerate, LazyList(false, true))
  }

  test("Universe.of") {
    val iInt = IndexOf((n: Int) => Nat(n))
    val gInt = Get(k => Some(k.toInt))
    assertEquals(Universe.of(LazyList.from(0), iInt, gInt).enumerate.take(2), LazyList(0, 1))
    assertEquals(Universe.of(LazyList.from(0), iInt, gInt).cardinality, Inf)
    assertEquals(Universe.of(LazyList.from(0), iInt, gInt).indexOf(0), Nat.Zero)
    assertEquals(Universe.of(LazyList.from(0), iInt, gInt).get(Nat.Zero), Some(0))
    assertEquals(Universe.of(LazyList(0, 1), Two, iInt, gInt).enumerate, LazyList(0, 1))
    assertEquals(Universe.of(LazyList(0, 1), Two, iInt, gInt).cardinality, Two)
    assertEquals(Universe.of(LazyList(0, 1), Two, iInt, gInt).indexOf(0), Nat.Zero)
    assertEquals(Universe.of(LazyList(0, 1), Two, iInt, gInt).get(Nat.Zero), Some(0))
  }

  test("Universe.finite") {
    assertEquals(Universe.finite[Unit].enumerate, LazyList(()))
    assertEquals(Universe.finite[Unit].cardinality, One)
  }

  test("Universe.bigInt") {
    assertEquals(Universe.bigInt.enumerate.take(5), LazyList[BigInt](0, -1, 1, -2, 2))
    assertEquals(Universe.bigInt.cardinality, Inf)
  }

  test("Universe.string") {
    assertEquals(Universe.string.enumerate.take(3), LazyList("", "\u0000", "\u0001"))
    assertEquals(Universe.string.cardinality, Inf)
  }

  test("Universe.tuple2") {
    assertEquals(
      Universe.tuple2(Universe[Boolean], Universe[BigInt]).enumerate.take(4),
      LazyList[(Boolean, BigInt)]((false, 0), (true, 0), (false, -1), (true, -1))
    )
    assertEquals(Universe.tuple2(Universe[Boolean], Universe[BigInt]).cardinality, Inf)
    assertEquals(Universe.tuple2(Universe[Boolean], Universe[Boolean]).cardinality, Small(4))
  }

  test("Universe.seq") {
    assertEquals(
      Universe.seq(Universe[BigInt]).enumerate.take(4),
      LazyList[List[BigInt]](List.empty, List(0), List(-1), List(0, 0))
    )
    assertEquals(Universe.seq(Universe[BigInt]).cardinality, Inf)
  }

  test("Universe.map") {
    assertEquals(
      Universe.map(Finite[Boolean], Universe[BigInt]).enumerate.take(4),
      LazyList[Map[Boolean, BigInt]](Map.empty, Map(false -> 0), Map(true -> 0), Map(false -> -1))
    )
    assertEquals(Universe.map(Finite[Boolean], Universe[BigInt]).cardinality, Inf)
    assertEquals(Universe.map(Finite[Boolean], Universe[Boolean]).cardinality, Small(9))
  }

  test("Universe.option") {
    assertEquals(
      Universe.option(Universe[BigInt]).enumerate.take(4),
      LazyList[Option[BigInt]](None, Some(0), Some(-1), Some(1))
    )
    assertEquals(Universe.option(Universe[BigInt]).cardinality, Inf)
    assertEquals(Universe.option(Universe[Boolean]).cardinality, Small(3))
  }

  test("Universe.either") {
    assertEquals(
      Universe.either(Universe[Boolean], Universe[BigInt]).enumerate.take(4),
      LazyList[Either[Boolean, BigInt]](Left(false), Right(0), Left(true), Right(-1))
    )
    assertEquals(Universe.either(Universe[Boolean], Universe[BigInt]).cardinality, Inf)
    assertEquals(Universe.either(Universe[Boolean], Universe[Boolean]).cardinality, Small(4))
  }

  test("Universe.function1") {
    assertEquals(
      Universe.function1(Finite[Boolean], Universe[BigInt]).enumerate.take(4),
      LazyList[Map[Boolean, BigInt]](
        Map.empty[Boolean, BigInt],
        Map(false -> -1),
        Map(true -> -1),
        Map(false -> 1)
      )
    )
    assertEquals(Universe.function1(Finite[Boolean], Universe[BigInt]).cardinality, Inf)
    assertEquals(Universe.function1(Finite[Boolean], Universe[Boolean]).cardinality, Small(4))
  }

  test("Universe.partialFunction") {
    assertEquals(
      Universe.partialFunction(Finite[Boolean], Universe[BigInt]).enumerate.take(4),
      LazyList[Map[Boolean, BigInt]](Map.empty, Map(false -> 0), Map(true -> 0), Map(false -> -1))
    )
    assertEquals(Universe.partialFunction(Finite[Boolean], Universe[BigInt]).cardinality, Inf)
    assertEquals(Universe.partialFunction(Finite[Boolean], Universe[Boolean]).cardinality, Small(9))
  }
}
