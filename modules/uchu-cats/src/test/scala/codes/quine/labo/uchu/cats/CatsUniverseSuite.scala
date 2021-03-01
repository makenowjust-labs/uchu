package codes.quine.labo.uchu.cats

import cats.Eval
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptySeq
import cats.data.NonEmptyVector

import codes.quine.labo.uchu.Card._
import codes.quine.labo.uchu.Nat
import codes.quine.labo.uchu.Universe

class CatsUniverseSuite extends munit.FunSuite {
  test("CatsUniverse.uchuUniverseForCatsEval") {
    val univ = CatsUniverse.uchuUniverseForCatsEval[Int]
    assertEquals(univ.card, Universe[Int].card)
    assertEquals(univ.indexOf(Eval.now(1)), Nat(2))
    assertEquals(univ.get(Nat(2)), Some(Eval.now(1)))
  }

  test("CatsUniverse.uchuUniverseForCatsDataChain") {
    val univ = CatsUniverse.uchuUniverseForCatsDataChain[Int]
    assertEquals(univ.card, Inf)
    assertEquals(univ.indexOf(Chain(0)), Nat(1))
    assertEquals(univ.get(Nat(1)), Some(Chain(0)))
  }

  test("CatsUniverse.uchuUniverseForCatsDataNonEmptyChain") {
    val univ = CatsUniverse.uchuUniverseForCatsDataNonEmptyChain[Int]
    assertEquals(univ.card, Inf)
    assertEquals(univ.indexOf(NonEmptyChain(1)), Nat(3))
    assertEquals(univ.get(Nat(3)), Some(NonEmptyChain(1)))
  }

  test("CatsUniverse.uchuUniverseForCatsDataNonEmptySeq") {
    val univ = CatsUniverse.uchuUniverseForCatsDataNonEmptySeq[Int]
    assertEquals(univ.card, Inf)
    assertEquals(univ.indexOf(NonEmptySeq.of(1)), Nat(3))
    assertEquals(univ.get(Nat(3)), Some(NonEmptySeq.of(1)))
  }

  test("CatsUniverse.uchuUniverseForCatsDataNonEmptyList") {
    val univ = CatsUniverse.uchuUniverseForCatsDataNonEmptyList[Int]
    assertEquals(univ.card, Inf)
    assertEquals(univ.indexOf(NonEmptyList.of(1)), Nat(3))
    assertEquals(univ.get(Nat(3)), Some(NonEmptyList.of(1)))
  }

  test("CatsUniverse.uchuUniverseForCatsDataNonEmptyVector") {
    val univ = CatsUniverse.uchuUniverseForCatsDataNonEmptyVector[Int]
    assertEquals(univ.card, Inf)
    assertEquals(univ.indexOf(NonEmptyVector.of(1)), Nat(3))
    assertEquals(univ.get(Nat(3)), Some(NonEmptyVector.of(1)))
  }

  test("CatsUniverse.uchuUniverseForCatsDataNonEmptyMap") {
    val univ = CatsUniverse.uchuUniverseForCatsDataNonEmptyMap[Int, BigInt]
    assertEquals(univ.card, Inf)
    assertEquals(univ.indexOf(NonEmptyMap.of(1 -> 2)), Nat(32))
    assertEquals(univ.get(Nat(32)), Some(NonEmptyMap.of(1 -> BigInt(2))))
  }

  test("CatsUniverse.uchuUniverseForCatsDataIor") {
    val univ = CatsUniverse.uchuUniverseForCatsDataIor[Boolean, Boolean]
    assertEquals(univ.card, Small(8))
    for ((v, k) <- univ.enumerate.zipWithIndex) {
      assertEquals(univ.get(Nat(k)), Some(v))
      assertEquals(univ.indexOf(v), Nat(k))
    }
  }
}
