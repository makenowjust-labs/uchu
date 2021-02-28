package codes.quine.labo.uchu.shapeless

import shapeless.:+:
import shapeless.::
import shapeless.CNil
import shapeless.HNil
import shapeless.Inl

import codes.quine.labo.uchu.Card._
import codes.quine.labo.uchu.Nat
import codes.quine.labo.uchu.Universe

class MkUniverseSuite extends munit.FunSuite {
  test("MkUniverse.mkUniverseHNil") {
    val univ = MkUniverse[HNil]
    assertEquals(univ.enumerate, LazyList(HNil))
    assertEquals(univ.indexOf(HNil), Nat(0))
    assertEquals(univ.get(Nat(0)), Some(HNil))
    assertEquals(univ.card, One)
  }

  test("MkUniverse.mkUniverseCNil") {
    val univ = MkUniverse[CNil]
    assertEquals(univ.enumerate, LazyList.empty)
    assertEquals(univ.card, Zero)
  }

  test("MkUniverse.mkUniverseHCons") {
    val univ = MkUniverse[Int :: HNil]
    assertEquals(univ.enumerate.take(3), LazyList(0 :: HNil, -1 :: HNil, 1 :: HNil))
    assertEquals(univ.indexOf(-1 :: HNil), Nat(1))
    assertEquals(univ.get(Nat(1)), Some(-1 :: HNil))
    assertEquals(univ.card, Universe[Int].card)
  }

  test("MkUniverse.mkUniverseCCons") {
    val univ = MkUniverse[Int :+: CNil]
    assertEquals(univ.enumerate.take(3), LazyList(Inl(0), Inl(-1), Inl(1)))
    assertEquals(univ.indexOf(Inl(-1)), Nat(1))
    assertEquals(univ.get(Nat(1)), Some(Inl(-1)))
    assertEquals(univ.card, Universe[Int].card)
  }

  test("MkUniverse.mkUniverseGeneric") {
    sealed abstract class FooBar
    final case class Foo(x: Int, y: Int) extends FooBar
    final case class Bar(x: Int) extends FooBar

    val univ = MkUniverse[FooBar]
    assertEquals(univ.enumerate.take(3), LazyList(Bar(0), Foo(0, 0), Bar(-1)))
    assertEquals(univ.indexOf(Foo(1, 2)), Nat(51))
    assertEquals(univ.get(Nat(50)), Some(Bar(-13)))
    assertEquals(univ.card, Universe[Either[(Int, Int), Int]].card)
  }
}
