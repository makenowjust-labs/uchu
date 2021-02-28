package codes.quine.labo.uchu.cats

import scala.util.Random

import cats.Eq
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
import codes.quine.labo.uchu.cats.CatsInstances._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class CardInstancesSuite extends munit.FunSuite with munit.DisciplineSuite {
  implicit def scalapropsArbitraryForUniverse[A](implicit A: Finite[A]): Arbitrary[Universe[A]] =
    Arbitrary(Arbitrary.arbitrary[Finite[A]].map(_.asInstanceOf[Universe[A]]))

  implicit def scalapropsArbitraryForFinite[A](implicit A: Finite[A]): Arbitrary[Finite[A]] =
    Arbitrary(for {
      seed <- Gen.long
      xs = new Random(seed).shuffle(A.enumerate)
      i = IndexOf[A](x => Nat(xs.indexOf(x)))
      g = Get[A](k => xs.lift(k.toInt))
    } yield Finite.of(xs, A.cardinality, i, g))

  implicit def scalapropsArbitraryForGet[A](implicit A: Finite[A]): Arbitrary[Get[A]] =
    Arbitrary(for {
      seed <- Gen.long
      f = new Random(seed).shuffle(A.enumerate).toVector.lift
    } yield Get[A](k => f(k.toInt)))

  implicit def scalapropsArbitraryForIndexOf[A](implicit A: Finite[A]): Arbitrary[IndexOf[A]] =
    Arbitrary(for {
      seed <- Gen.long
      f = new Random(seed).shuffle(A.enumerate).zipWithIndex.toMap
    } yield IndexOf[A](x => Nat(f(x))))

  implicit val catsEqForNat: Eq[Nat] = Eq.fromUniversalEquals
  implicit val catsEqForCard: Eq[Card] = Eq.fromUniversalEquals

  implicit def catsEqForUniverse[A](implicit fin: Finite[A], eq: Eq[A]): Eq[Universe[A]] =
    Eq.by(fin => (fin.enumerate, fin.cardinality, fin.indexOf, fin.get))

  implicit def catsEqForFinite[A](implicit fin: Finite[A], eq: Eq[A]): Eq[Finite[A]] =
    Eq.by(_.asInstanceOf[Universe[A]])

  implicit def catsEqForGet[A](implicit fin: Finite[A], eq: Eq[A]): Eq[Get[A]] =
    Eq.by(Enumerate.to(fin.cardinality).map(_))

  implicit def catsEqForIndexOf[A](implicit A: Finite[A]): Eq[IndexOf[A]] =
    Eq.by(A.enumerate.map(_))

  checkAll("Universe", InvariantTests[Universe].invariant[Byte, Byte, Byte])
  checkAll("Finite", InvariantTests[Finite].invariant[Byte, Byte, Byte])
  checkAll("Get", FunctorTests[Get].functor[Byte, Byte, Byte])
  checkAll("IndexOf", ContravariantTests[IndexOf].contravariant[Byte, Byte, Byte])
}
