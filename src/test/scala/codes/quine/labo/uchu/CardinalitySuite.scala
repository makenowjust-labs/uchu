package codes.quine.labo.uchu

import codes.quine.labo.uchu.Cardinality._

class CardinalitySuite extends munit.FunSuite {
  test("Cardinality#+") {
    assertEquals(Fin(1) + Fin(2), Fin(3))
    assertEquals(Fin(1) + Inf, Inf)
    assertEquals(Inf + Fin(2), Inf)
    assertEquals(Inf + Inf, Inf)
    assertEquals(Fin(1) + 2, Fin(3))
  }

  test("Cardinality#*") {
    assertEquals(Fin(1) * Fin(2), Fin(2))
    assertEquals(Fin(1) * Inf, Inf)
    assertEquals(Inf * Fin(2), Inf)
    assertEquals(Inf * Inf, Inf)
  }

  test("Cardinality#**") {
    assertEquals(Fin(2) ** Fin(2), Fin(4))
    assertEquals(Inf ** Fin(2), Inf)
    intercept[ArithmeticException](Fin(2) ** Fin((2: BigInt).pow(32)))
  }
}
