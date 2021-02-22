package codes.quine.labo.uchu

class NatSuite extends munit.FunSuite {
  test("Nat#bit") {
    assertEquals(Nat(0).bit(0), false)
    assertEquals(Nat(1).bit(0), true)
    assertEquals(Nat(10).bit(0), false)
    assertEquals(Nat(10).bit(1), true)
  }

  test("Nat#bitSize") {
    assertEquals(Nat(0).bitSize, 0)
    assertEquals(Nat(1).bitSize, 1)
    assertEquals(Nat(10).bitSize, 4)
  }

  test("Nat#bits") {
    assertEquals(Nat(0).bits, LazyList.empty[Boolean])
    assertEquals(Nat(1).bits, LazyList(true))
    assertEquals(Nat(10).bits, LazyList(false, true, false, true))
  }

  test("Nat#+") {
    assertEquals(Nat(1) + Nat(2), Nat(3))
    assertEquals(Nat(1) + 2, Nat(3))
  }

  test("Nat#-") {
    assertEquals(Nat(2) - Nat(1), Nat(1))
    assertEquals(Nat(2) - 1, Nat(1))
  }

  test("Nat#*") {
    assertEquals(Nat(2) * Nat(3), Nat(6))
    assertEquals(Nat(2) * 3, Nat(6))
  }

  test("Nat#/") {
    assertEquals(Nat(4) / Nat(2), Nat(2))
    assertEquals(Nat(4) / 2, Nat(2))
  }

  test("Nat#/%") {
    assertEquals(Nat(10) /% Nat(3), (Nat(3), Nat(1)))
  }

  test("Nat#**") {
    assertEquals(Nat(4) ** Nat(2), Nat(16))
    assertEquals(Nat(4) ** 2, Nat(16))
  }

  test("Nat#|") {
    assertEquals(Nat(10) | Nat(5), Nat(15))
    assertEquals(Nat(10) | 5, Nat(15))
  }

  test("Nat#<<") {
    assertEquals(Nat(1) << Nat(4), Nat(16))
    assertEquals(Nat(1) << 4, Nat(16))
  }

  test("Nat#toInt") {
    assertEquals(Nat(1).toInt, 1)
    intercept[ArithmeticException](Nat((2: BigInt).pow(32)).toInt)
  }

  test("N.sqrt") {
    assertEquals(Nat.sqrt(Nat(0)), Nat(0))
    assertEquals(Nat.sqrt(Nat(1)), Nat(1))
    assertEquals(Nat.sqrt(Nat(14)), Nat(3))
    assertEquals(Nat.sqrt(Nat(100)), Nat(10))
    assertEquals(Nat.sqrt(Nat((2: BigInt).pow(100))), Nat((2: BigInt).pow(50)))
    assertEquals(Nat.sqrt(Nat((2: BigInt).pow(100) - 1)), Nat((2: BigInt).pow(50) - 1))
  }
}
