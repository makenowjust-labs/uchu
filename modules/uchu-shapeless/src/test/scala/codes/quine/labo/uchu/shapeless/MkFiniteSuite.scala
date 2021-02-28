package codes.quine.labo.uchu.shapeless

import shapeless.:+:
import shapeless.::
import shapeless.CNil
import shapeless.HNil
import shapeless.Inl

import codes.quine.labo.uchu.Card._
import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.Nat

class MkFiniteSuite extends munit.FunSuite {
  test("MkFinite.mkFiniteHNil") {
    val fin = MkFinite[HNil]
    assertEquals(fin.enumerate, LazyList(HNil))
    assertEquals(fin.indexOf(HNil), Nat(0))
    assertEquals(fin.get(Nat(0)), Some(HNil))
    assertEquals(fin.card, One)
  }

  test("MkFinite.mkFiniteCNil") {
    val fin = MkFinite[CNil]
    assertEquals(fin.enumerate, LazyList.empty)
    assertEquals(fin.card, Zero)
  }

  test("MkFinite.mkFiniteHCons") {
    val fin = MkFinite[Int :: HNil]
    assertEquals(fin.enumerate.take(3), LazyList(0 :: HNil, -1 :: HNil, 1 :: HNil))
    assertEquals(fin.indexOf(-1 :: HNil), Nat(1))
    assertEquals(fin.get(Nat(1)), Some(-1 :: HNil))
    assertEquals(fin.card, Finite[Int].card)
  }

  test("MkFinite.mkFiniteCCons") {
    val fin = MkFinite[Int :+: CNil]
    assertEquals(fin.enumerate.take(3), LazyList(Inl(0), Inl(-1), Inl(1)))
    assertEquals(fin.indexOf(Inl(-1)), Nat(1))
    assertEquals(fin.get(Nat(1)), Some(Inl(-1)))
    assertEquals(fin.card, Finite[Int].card)
  }

  test("MkFinite.mkFiniteGeneric") {
    sealed abstract class FooBar
    final case class Foo(x: Int, y: Int) extends FooBar
    final case class Bar(x: Int) extends FooBar

    val fin = MkFinite[FooBar]
    assertEquals(fin.enumerate.take(3), LazyList(Bar(0), Foo(0, 0), Bar(-1)))
    assertEquals(fin.indexOf(Foo(1, 2)), Nat(51))
    assertEquals(fin.get(Nat(50)), Some(Bar(-13)))
    assertEquals(fin.card, Finite[Either[(Int, Int), Int]].card)
  }
}
