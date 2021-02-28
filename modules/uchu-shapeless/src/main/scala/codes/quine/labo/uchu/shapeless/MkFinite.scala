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

import codes.quine.labo.uchu.Card._
import codes.quine.labo.uchu.Enumerate
import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.Get
import codes.quine.labo.uchu.IndexOf
import codes.quine.labo.uchu.Nat

/** MkFinite is a marker class for Finite instance derivation. */
trait MkFinite[A] extends Finite[A]

/** MkFinite utilities. */
object MkFinite extends MkFiniteDerivation {

  /** Derives a Finite instance for A. */
  def apply[A](implicit A: MkFinite[A]): MkFinite[A] = A
}

/** MkFinite derivations. */
private[shapeless] abstract class MkFiniteDerivation {

  /** A MkFinite instance for HNil. */
  implicit val mkFiniteHNil: MkFinite[HNil] = new MkFinite[HNil] {
    def enumerate: LazyList[HNil] = LazyList(HNil)
    def get: Get[HNil] = Get(k => if (k == Nat.Zero) Some(HNil) else None)
    def indexOf: IndexOf[HNil] = IndexOf(_ => Nat.Zero)
    def card: Fin = One
  }

  /** A MkFinite instance for CNil. */
  implicit val mkFiniteCNil: MkFinite[CNil] = new MkFinite[CNil] {
    def enumerate: LazyList[CNil] = LazyList.empty
    def get: Get[CNil] = Get(_ => None)
    def indexOf: IndexOf[CNil] = IndexOf(_ => throw new NoSuchElementException)
    def card: Fin = Zero
  }

  /** A MkFinite instance for HCons. */
  implicit def mkFiniteHCons[H, T <: HList](implicit
      H: Finite[H] OrElse MkFinite[H],
      T: MkFinite[T]
  ): MkFinite[H :: T] =
    new MkFinite[H :: T] {
      def enumerate: LazyList[H :: T] =
        Enumerate.tuple2(H.unify.enumerate, T.enumerate).map { case (x, xs) => x :: xs }
      def get: Get[H :: T] =
        Get.tuple2(H.unify.get, H.unify.card, T.get, T.card).map { case (x, xs) => x :: xs }
      def indexOf: IndexOf[H :: T] =
        IndexOf.tuple2(H.unify.indexOf, H.unify.card, T.indexOf, T.card).contramap { case x :: xs => (x, xs) }
      def card: Fin = H.unify.card * T.card
    }

  /** A MkFinite instance for CCons. */
  implicit def mkFiniteCCons[H, T <: Coproduct](implicit
      H: Finite[H] OrElse MkFinite[H],
      T: MkFinite[T]
  ): MkFinite[H :+: T] =
    new MkFinite[H :+: T] {
      def enumerate: LazyList[H :+: T] =
        Enumerate.either(H.unify.enumerate, T.enumerate).map(_.fold(Inl(_), Inr(_)))
      def get: Get[H :+: T] =
        Get.either(H.unify.get, H.unify.card, T.get, T.card).map(_.fold(Inl(_), Inr(_)))
      def indexOf: IndexOf[H :+: T] =
        IndexOf.either(H.unify.indexOf, H.unify.card, T.indexOf, T.card).contramap(_.eliminate(Left(_), Right(_)))
      def card: Fin = H.unify.card + T.card
    }

  /** A MkFinite instance for Generic. */
  implicit def mkFiniteGeneric[A, R](implicit A: Generic.Aux[A, R], R: MkFinite[R]): MkFinite[A] =
    new MkFinite[A] {
      def enumerate: LazyList[A] = R.enumerate.map(A.from)
      def get: Get[A] = R.get.map(A.from)
      def indexOf: IndexOf[A] = R.indexOf.contramap(A.to)
      def card: Fin = R.card
    }
}
