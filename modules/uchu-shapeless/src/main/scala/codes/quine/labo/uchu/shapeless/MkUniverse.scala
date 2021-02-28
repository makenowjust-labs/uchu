package codes.quine.labo.uchu.shapeless

import shapeless.:+:
import shapeless.::
import shapeless.CNil
import shapeless.Coproduct
import shapeless.Generic
import shapeless.HList
import shapeless.HNil
import shapeless.Inl
import shapeless.Inr
import shapeless.OrElse

import codes.quine.labo.uchu.Card
import codes.quine.labo.uchu.Card._
import codes.quine.labo.uchu.Enumerate
import codes.quine.labo.uchu.Get
import codes.quine.labo.uchu.IndexOf
import codes.quine.labo.uchu.Nat
import codes.quine.labo.uchu.Universe

/** MkUniverse is a marker class for Universe derivation. */
trait MkUniverse[A] extends Universe[A]

/** MkUniverse utilities. */
object MkUniverse extends MkUniverseDerivation {

  /** Derives a Universe instance for A. */
  def apply[A](implicit A: MkUniverse[A]): MkUniverse[A] = A
}

/** MkUniverse derivations. */
private[shapeless] abstract class MkUniverseDerivation {

  /** A MkUniverse instance for HNil. */
  implicit val mkUniverseHNil: MkUniverse[HNil] = new MkUniverse[HNil] {
    def enumerate: LazyList[HNil] = LazyList(HNil)
    def get: Get[HNil] = Get(k => if (k == Nat.Zero) Some(HNil) else None)
    def indexOf: IndexOf[HNil] = IndexOf(_ => Nat.Zero)
    def card: Card = One
  }

  /** A MkUniverse instance for CNil. */
  implicit val mkUniverseCNil: MkUniverse[CNil] = new MkUniverse[CNil] {
    def enumerate: LazyList[CNil] = LazyList.empty
    def get: Get[CNil] = Get(_ => None)
    def indexOf: IndexOf[CNil] = IndexOf(_ => throw new NoSuchElementException)
    def card: Card = Zero
  }

  /** A MkUniverse instance for HCons. */
  implicit def mkUniverseHCons[H, T <: HList](implicit
      H: Universe[H] OrElse MkUniverse[H],
      T: MkUniverse[T]
  ): MkUniverse[H :: T] =
    new MkUniverse[H :: T] {
      def enumerate: LazyList[H :: T] =
        Enumerate.tuple2(H.unify.enumerate, T.enumerate).map { case (x, xs) => x :: xs }
      def get: Get[H :: T] =
        Get.tuple2(H.unify.get, H.unify.card, T.get, T.card).map { case (x, xs) => x :: xs }
      def indexOf: IndexOf[H :: T] =
        IndexOf.tuple2(H.unify.indexOf, H.unify.card, T.indexOf, T.card).contramap { case x :: xs => (x, xs) }
      def card: Card = H.unify.card * T.card
    }

  /** A MkUniverse instance for CCons. */
  implicit def mkUniverseCCons[H, T <: Coproduct](implicit
      H: Universe[H] OrElse MkUniverse[H],
      T: MkUniverse[T]
  ): MkUniverse[H :+: T] =
    new MkUniverse[H :+: T] {
      def enumerate: LazyList[H :+: T] =
        Enumerate.either(H.unify.enumerate, T.enumerate).map(_.fold(Inl(_), Inr(_)))
      def get: Get[H :+: T] =
        Get.either(H.unify.get, H.unify.card, T.get, T.card).map(_.fold(Inl(_), Inr(_)))
      def indexOf: IndexOf[H :+: T] =
        IndexOf.either(H.unify.indexOf, H.unify.card, T.indexOf, T.card).contramap(_.eliminate(Left(_), Right(_)))
      def card: Card = H.unify.card + T.card
    }

  /** A MkUniverse instance for Generic.
    *
    * Intentionally this does not support recursive type because it is hard to derive an instance for such a type.
    */
  implicit def mkUniverseGeneric[A, R](implicit A: Generic.Aux[A, R], R: MkUniverse[R]): MkUniverse[A] =
    new MkUniverse[A] {
      def enumerate: LazyList[A] = R.enumerate.map(A.from)
      def get: Get[A] = R.get.map(A.from)
      def indexOf: IndexOf[A] = R.indexOf.contramap(A.to)
      def card: Card = R.card
    }
}

sealed abstract class FooBar
final case class Foo(x: Int, y: Int) extends FooBar
final case class Bar(x: Int) extends FooBar
