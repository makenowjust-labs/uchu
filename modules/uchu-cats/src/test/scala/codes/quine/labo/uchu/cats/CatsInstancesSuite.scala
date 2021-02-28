package codes.quine.labo.uchu.cats

import scala.util.Random

import cats.Eq
import cats.kernel.laws.discipline.LowerBoundedTests
import cats.kernel.laws.discipline.OrderTests
import cats.laws.discipline.ContravariantTests
import cats.laws.discipline.FunctorTests
import cats.laws.discipline.InvariantTests

import codes.quine.labo.uchu.Card
import codes.quine.labo.uchu.Card._
import codes.quine.labo.uchu.Enumerate
import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.Get
import codes.quine.labo.uchu.IndexOf
import codes.quine.labo.uchu.Nat
import codes.quine.labo.uchu.Universe
import codes.quine.labo.uchu.cats.CatsInstances._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

class CatsInstancesSuite extends munit.FunSuite with munit.DisciplineSuite {
  implicit def scalapropsArbitraryForNat: Arbitrary[Nat] =
    Arbitrary(Arbitrary.arbitrary[BigInt].map(x => if (x < 0) Nat(-x) else Nat(x)))

  implicit def scalapropsArbitraryForCard: Arbitrary[Card] =
    Arbitrary(
      Gen.frequency(
        18 -> Arbitrary.arbitrary[Nat].map(Small(_)),
        1 -> Gen.const(TooLarge),
        1 -> Gen.const(Inf)
      )
    )

  implicit def scalapropsCogenForNat: Cogen[Nat] = Cogen[BigInt].contramap(_.value)
  implicit def scalapropsCogenForCard: Cogen[Card] = Cogen[Either[Nat, Boolean]].contramap {
    case Small(n) => Left(n)
    case TooLarge => Right(false)
    case Inf      => Right(true)
  }

  implicit def scalapropsArbitraryForUniverse[A](implicit A: Finite[A]): Arbitrary[Universe[A]] =
    Arbitrary(Arbitrary.arbitrary[Finite[A]].map(_.asInstanceOf[Universe[A]]))

  implicit def scalapropsArbitraryForFinite[A](implicit A: Finite[A]): Arbitrary[Finite[A]] =
    Arbitrary(for {
      seed <- Gen.long
      xs = new Random(seed).shuffle(A.enumerate)
      i = IndexOf[A](x => Nat(xs.indexOf(x)))
      g = Get[A](k => xs.lift(k.toInt))
    } yield Finite.of(xs, A.card, i, g))

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

  implicit def catsEqForUniverse[A](implicit fin: Finite[A], eq: Eq[A]): Eq[Universe[A]] =
    Eq.by(fin => (fin.enumerate, fin.card, fin.indexOf, fin.get))

  implicit def catsEqForFinite[A](implicit fin: Finite[A], eq: Eq[A]): Eq[Finite[A]] =
    Eq.by(_.asInstanceOf[Universe[A]])

  implicit def catsEqForGet[A](implicit fin: Finite[A], eq: Eq[A]): Eq[Get[A]] =
    Eq.by(Enumerate.to(fin.card).map(_))

  implicit def catsEqForIndexOf[A](implicit A: Finite[A]): Eq[IndexOf[A]] =
    Eq.by(A.enumerate.map(_))

  checkAll("Universe", InvariantTests[Universe].invariant[Byte, Byte, Byte])
  checkAll("Finite", InvariantTests[Finite].invariant[Byte, Byte, Byte])
  checkAll("Get", FunctorTests[Get].functor[Byte, Byte, Byte])
  checkAll("IndexOf", ContravariantTests[IndexOf].contravariant[Byte, Byte, Byte])
  checkAll("Card", OrderTests[Card].order)
  checkAll("Nat", LowerBoundedTests[Nat].lowerBounded)
  checkAll("Nat", OrderTests[Nat].order)
}
