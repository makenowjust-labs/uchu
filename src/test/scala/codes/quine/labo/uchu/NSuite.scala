package codes.quine.labo.uchu

class NSuite extends munit.FunSuite {
  test("N#bit") {
    assertEquals(N(0).bit(0), false)
    assertEquals(N(1).bit(0), true)
    assertEquals(N(10).bit(0), false)
    assertEquals(N(10).bit(1), true)
  }

  test("N#bitSize") {
    assertEquals(N(0).bitSize, 0)
    assertEquals(N(1).bitSize, 1)
    assertEquals(N(10).bitSize, 4)
  }

  test("N#bits") {
    assertEquals(N(0).bits, LazyList.empty[Boolean])
    assertEquals(N(1).bits, LazyList(true))
    assertEquals(N(10).bits, LazyList(false, true, false, true))
  }

  test("N#+") {
    assertEquals(N(1) + N(2), N(3))
    assertEquals(N(1) + 2, N(3))
  }

  test("N#-") {
    assertEquals(N(2) - N(1), N(1))
    assertEquals(N(2) - 1, N(1))
  }

  test("N#*") {
    assertEquals(N(2) * N(3), N(6))
    assertEquals(N(2) * 3, N(6))
  }

  test("N#/") {
    assertEquals(N(4) / N(2), N(2))
    assertEquals(N(4) / 2, N(2))
  }

  test("N#**") {
    assertEquals(N(4) ** N(2), N(16))
    assertEquals(N(4) ** 2, N(16))
  }

  test("N#|") {
    assertEquals(N(10) | N(5), N(15))
    assertEquals(N(10) | 5, N(15))
  }

  test("N#<<") {
    assertEquals(N(1) << N(4), N(16))
    assertEquals(N(1) << 4, N(16))
  }

  test("N#toInt") {
    assertEquals(N(1).toInt, 1)
    intercept[ArithmeticException](N((2: BigInt).pow(32)).toInt)
  }

  test("N.sqrt") {
    assertEquals(N.sqrt(N(0)), N(0))
    assertEquals(N.sqrt(N(1)), N(1))
    assertEquals(N.sqrt(N(14)), N(3))
    assertEquals(N.sqrt(N(100)), N(10))
    assertEquals(N.sqrt(N((2: BigInt).pow(100))), N((2: BigInt).pow(50)))
    assertEquals(N.sqrt(N((2: BigInt).pow(100) - 1)), N((2: BigInt).pow(50) - 1))
  }
}
