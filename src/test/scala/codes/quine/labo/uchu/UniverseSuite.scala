package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

class UniverseSuite extends munit.FunSuite {
  test("Universe.apply") {
    assertEquals(Universe[Boolean].enumerate, LazyList(false, true))
  }

  test("Universe.of") {
    val iInt = IndexOf((n: Int) => N(n))
    assertEquals(Universe.of(LazyList.from(0), iInt).enumerate.take(2), LazyList(0, 1))
    assertEquals(Universe.of(LazyList.from(0), iInt).cardinality, Inf)
    assertEquals(Universe.of(LazyList.from(0), iInt).indexOf(0), N.Zero)
    assertEquals(Universe.of(LazyList(0, 1), Two, iInt).enumerate, LazyList(0, 1))
    assertEquals(Universe.of(LazyList(0, 1), Two, iInt).cardinality, Two)
    assertEquals(Universe.of(LazyList(0, 1), Two, iInt).indexOf(0), N.Zero)
  }

  test("Universe.finite") {
    assertEquals(Universe.finite[Unit].enumerate, LazyList(()))
    assertEquals(Universe.finite[Unit].cardinality, One)
  }

  test("Universe.bigInt") {
    assertEquals(Universe.bigInt.enumerate.take(5), LazyList[BigInt](0, -1, 1, -2, 2))
    assertEquals(Universe.bigInt.cardinality, Inf)
  }

  test("Universe.tuple2") {
    assertEquals(
      Universe.tuple2(Universe[Boolean], Universe[BigInt]).enumerate.take(4),
      LazyList[(Boolean, BigInt)]((false, 0), (true, 0), (false, -1), (true, -1))
    )
    assertEquals(Universe.tuple2(Universe[Boolean], Universe[BigInt]).cardinality, Inf)
    assertEquals(Universe.tuple2(Universe[Boolean], Universe[Boolean]).cardinality, Small(4))
  }

  test("Universe.list") {
    assertEquals(
      Universe.list(Universe[BigInt]).enumerate.take(4),
      LazyList[List[BigInt]](List.empty, List(0), List(-1), List(0, 0))
    )
    assertEquals(Universe.list(Universe[BigInt]).cardinality, Inf)
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
        Map(false -> 0, true -> 0),
        Map(false -> -1, true -> 0),
        Map(false -> 0, true -> -1),
        Map(false -> 1, true -> 0)
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
