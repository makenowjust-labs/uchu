package codes.quine.labo.uchu

class UniverseSuite extends munit.FunSuite {
  test("Universe.apply") {
    assertEquals(Universe[Boolean].universe, LazyList(false, true))
  }

  test("Universe.of") {
    assertEquals(Universe.of(LazyList(0, 1)).universe, LazyList(0, 1))
  }

  test("Universe.finite") {
    assertEquals(Universe.finite[Unit].universe, LazyList(()))
  }

  test("Universe.bigInt") {
    assertEquals(Universe.bigInt.universe.take(5), LazyList[BigInt](0, -1, 1, -2, 2))
  }

  test("Universe.tuple2") {
    assertEquals(
      Universe.tuple2(Universe[Boolean], Universe[BigInt]).universe.take(4),
      LazyList[(Boolean, BigInt)]((false, 0), (true, 0), (false, -1), (true, -1))
    )
  }

  test("Universe.list") {
    assertEquals(
      Universe.list(Universe[BigInt]).universe.take(4),
      LazyList[List[BigInt]](List.empty, List(0), List(-1), List(0, 0))
    )
  }

  test("Universe.map") {
    assertEquals(
      Universe.map(Finite[Boolean], Universe[BigInt]).universe.take(4),
      LazyList[Map[Boolean, BigInt]](Map.empty, Map(false -> 0), Map(true -> 0), Map(false -> -1))
    )
  }

  test("Universe.option") {
    assertEquals(
      Universe.option(Universe[BigInt]).universe.take(4),
      LazyList[Option[BigInt]](None, Some(0), Some(-1), Some(1))
    )
  }

  test("Universe.either") {
    assertEquals(
      Universe.either(Universe[Boolean], Universe[BigInt]).universe.take(4),
      LazyList[Either[Boolean, BigInt]](Left(false), Right(0), Left(true), Right(-1))
    )
  }

  test("Universe.function2") {
    assertEquals(
      Universe.function2(Finite[Boolean], Universe[BigInt]).universe.take(4),
      LazyList[Map[Boolean, BigInt]](
        Map(false -> 0, true -> 0),
        Map(false -> -1, true -> 0),
        Map(false -> 0, true -> -1),
        Map(false -> 1, true -> 0)
      )
    )
  }

  test("Universe.partialFunction") {
    assertEquals(
      Universe.partialFunction(Finite[Boolean], Universe[BigInt]).universe.take(4),
      LazyList[Map[Boolean, BigInt]](Map.empty, Map(false -> 0), Map(true -> 0), Map(false -> -1))
    )
  }
}
