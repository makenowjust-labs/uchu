package codes.quine.labo.uchu.laws

import scala.util.Random

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import codes.quine.labo.uchu.Card
import codes.quine.labo.uchu.Card._
import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.Get
import codes.quine.labo.uchu.IndexOf
import codes.quine.labo.uchu.Nat
import codes.quine.labo.uchu.Universe

/** Scalacheck's generators/co-generators for uchu types. */
trait UchuArbitrary {

  /** ScalaCheck generator for Nat. */
  implicit def scalacheckArbitraryForUchuNat: Arbitrary[Nat] =
    Arbitrary(Arbitrary.arbitrary[BigInt].map(x => Nat(x.abs)))

  /** ScalaCheck generator for Card. */
  implicit def scalacheckArbitraryForUchuCard: Arbitrary[Card] =
    Arbitrary(
      Gen.frequency(
        18 -> Arbitrary.arbitrary[Nat].map(Small(_)),
        1 -> Gen.const(TooLarge),
        1 -> Gen.const(Inf)
      )
    )

  /** ScalaCheck generator for Universe. */
  implicit def scalacheckArbitraryForUchuUniverse[A](implicit A: Finite[A]): Arbitrary[Universe[A]] =
    Arbitrary(Arbitrary.arbitrary[Finite[A]].map(_.asInstanceOf[Universe[A]]))

  /** ScalaCheck generator for Finite. */
  implicit def scalacheckArbitraryForUchuFinite[A](implicit A: Finite[A]): Arbitrary[Finite[A]] =
    Arbitrary(for {
      seed <- Gen.long
      xs = new Random(seed).shuffle(A.enumerate)
      i = IndexOf[A](x => Nat(xs.indexOf(x)))
      g = Get[A](k => xs.lift(k.toInt))
    } yield Finite.of(xs, A.card, i, g))

  /** ScalaCheck generator for Get. */
  implicit def scalacheckArbitraryForUchuGet[A](implicit A: Finite[A]): Arbitrary[Get[A]] =
    Arbitrary(for {
      seed <- Gen.long
      f = new Random(seed).shuffle(A.enumerate).toVector.lift
    } yield Get[A](k => f(k.toInt)))

  /** ScalaCheck generator for IndexOf. */
  implicit def scalacheckArbitraryForUchuIndexOf[A](implicit A: Finite[A]): Arbitrary[IndexOf[A]] =
    Arbitrary(for {
      seed <- Gen.long
      f = new Random(seed).shuffle(A.enumerate).zipWithIndex.toMap
    } yield IndexOf[A](x => Nat(f(x))))

  /** ScalaCheck co-generator for Nat. */
  implicit def scalacheckCogenForUchuNat: Cogen[Nat] = Cogen[BigInt].contramap(_.value)

  /** ScalaCheck co-generator for Card. */
  implicit def scalacheckCogenForUchuCard: Cogen[Card] = Cogen[Either[Nat, Boolean]].contramap {
    case Small(n) => Left(n)
    case TooLarge => Right(false)
    case Inf      => Right(true)
  }
}

object UchuArbitrary extends UchuArbitrary
