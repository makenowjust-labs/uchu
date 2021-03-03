package codes.quine.labo.uchu.laws

import cats.Eq
import cats.kernel.laws.discipline.OrderTests
import cats.laws.discipline.ContravariantTests
import cats.laws.discipline.FunctorTests
import cats.laws.discipline.InvariantTests

import codes.quine.labo.uchu.Card
import codes.quine.labo.uchu.Enumerate
import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.Get
import codes.quine.labo.uchu.IndexOf
import codes.quine.labo.uchu.Nat
import codes.quine.labo.uchu.Universe
import codes.quine.labo.uchu.cats.instances._
import codes.quine.labo.uchu.laws.UchuArbitrary._

class CatsLawsSuite extends munit.FunSuite with munit.DisciplineSuite {
  implicit def catsEqForUniverse[A](implicit fin: Finite[A], eq: Eq[A]): Eq[Universe[A]] =
    Eq.by(fin => (fin.enumerate, fin.card, fin.indexOf, fin.get))

  implicit def catsEqForFinite[A](implicit fin: Finite[A], eq: Eq[A]): Eq[Finite[A]] =
    Eq.by(_.asInstanceOf[Universe[A]])

  implicit def catsEqForGet[A](implicit fin: Finite[A], eq: Eq[A]): Eq[Get[A]] =
    Eq.by(Enumerate.to(fin.card).map(_))

  implicit def catsEqForIndexOf[A](implicit A: Finite[A]): Eq[IndexOf[A]] =
    Eq.by(A.enumerate.map(_))

  checkAll("Invariant[Universe]", InvariantTests[Universe].invariant[Byte, Byte, Byte])
  checkAll("Invariant[Finite]", InvariantTests[Finite].invariant[Byte, Byte, Byte])
  checkAll("Functor[Get]", FunctorTests[Get].functor[Byte, Byte, Byte])
  checkAll("Contravariant[IndexOf]", ContravariantTests[IndexOf].contravariant[Byte, Byte, Byte])
  checkAll("Order[Card]", OrderTests[Card].order)
  checkAll("Order[Nat]", OrderTests[Nat].order)
}
