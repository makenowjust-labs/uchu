package codes.quine.labo.uchu.cats

import cats.Eval

import codes.quine.labo.uchu.Card._
import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.Nat

class CatsFiniteSuite extends munit.FunSuite {
  test("CatsFinite.uchuFiniteForCatsEval") {
    val fin = CatsFinite.uchuFiniteForCatsEval[Int]
    assertEquals(fin.card, Finite[Int].card)
    assertEquals(fin.indexOf(Eval.now(1)), Nat(2))
    assertEquals(fin.get(Nat(2)), Some(Eval.now(1)))
  }

  test("CatsFinite.uchuFiniteForCatsDataIor") {
    val fin = CatsFinite.uchuFiniteForCatsDataIor[Boolean, Boolean]
    assertEquals(fin.card, Small(8))
    for ((v, k) <- fin.enumerate.zipWithIndex) {
      assertEquals(fin.get(Nat(k)), Some(v))
      assertEquals(fin.indexOf(v), Nat(k))
    }
  }

  test("CatsFinite.uchuFiniteForCatsDataCont") {
    val fin = CatsFinite.uchuFiniteForCatsDataCont[Boolean, Boolean]
    assertEquals(fin.card, Small(16))
    for ((v, k) <- fin.enumerate.zipWithIndex) {
      assertEquals(fin.get(Nat(k)), Some(v))
      assertEquals(fin.indexOf(v), Nat(k))
    }
  }
}
