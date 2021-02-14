package codes.quine.labo.uchu

import codes.quine.labo.uchu.Card._

class CardSuite extends munit.FunSuite {
  test("Card#+") {
    assertEquals(Small(1) + Small(2), Small(3))
    assertEquals(Small(1) + TooLarge, TooLarge)
    assertEquals(TooLarge + Small(2), TooLarge)
    assertEquals(TooLarge + TooLarge, TooLarge)
    assertEquals(Small(1) + Inf, Inf)
    assertEquals(Inf + Small(2), Inf)
    assertEquals(TooLarge + Inf, Inf)
    assertEquals(Inf + TooLarge, Inf)
    assertEquals(Inf + Inf, Inf)
    assertEquals(Small(1) + 2, Small(3))
    assertEquals(TooLarge + 2, TooLarge)
    assertEquals(Inf + 2, Inf)
  }

  test("Card#-") {
    assertEquals(Small(1) - 1, Small(0))
    assertEquals(TooLarge - 1, TooLarge)
    assertEquals(Inf - 1, Inf)
  }

  test("Card#*") {
    assertEquals(Small(1) * Small(2), Small(2))
    assertEquals(Small(1) * TooLarge, TooLarge)
    assertEquals(TooLarge * Small(2), TooLarge)
    assertEquals(TooLarge * TooLarge, TooLarge)
    assertEquals(TooLarge * Zero, Zero)
    assertEquals(Zero * TooLarge, Zero)
    assertEquals(Small(1) * Inf, Inf)
    assertEquals(Inf * Small(2), Inf)
    assertEquals(TooLarge * Inf, Inf)
    assertEquals(Inf * TooLarge, Inf)
    assertEquals(Inf * Inf, Inf)
  }

  test("Card#**") {
    assertEquals(Small(2) ** Small(2), Small(4))
    assertEquals(TooLarge ** Small(2), TooLarge)
    assertEquals(Small(2) ** TooLarge, TooLarge)
    assertEquals(Zero ** TooLarge, Zero)
    assertEquals(TooLarge ** Zero, One)
    assertEquals(Inf ** Small(2), Inf)
    assertEquals(Small(2) ** Small((2: BigInt).pow(32)), TooLarge)
  }
}
