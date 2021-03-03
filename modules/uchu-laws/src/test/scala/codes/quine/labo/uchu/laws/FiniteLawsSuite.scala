package codes.quine.labo.uchu.laws

import cats.Eval
import cats.data.Cont
import cats.data.Ior
import cats.data.NonEmptyMap
import cats.data.NonEmptySet
import cats.kernel.Eq
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.cats.instances._
import codes.quine.labo.uchu.shapeless.semiauto._

class FiniteLawsSuite extends munit.FunSuite with munit.DisciplineSuite {
  implicit def scalacheckArbitraryForCatsDataCont[A: Arbitrary: Cogen, B: Arbitrary]: Arbitrary[Cont[A, B]] =
    Arbitrary(Arbitrary.arbitrary[(B => Eval[A]) => Eval[A]].map(Cont(_)))

  implicit def catsKernelEqForFunction1[A, B](implicit A: Finite[A], B: Eq[B]): Eq[A => B] =
    Eq.by(A.enumerate.map(_))

  implicit def catsKernelEqForPartialFunction[A, B](implicit A: Finite[A], B: Eq[B]): Eq[PartialFunction[A, B]] =
    Eq.by(f => A.enumerate.map(f.lift))

  implicit def catsKernelEqForCont[A: Finite: Eq, B: Finite]: Eq[Cont[A, B]] = {
    val BA = Finite[B => Eval[A]]
    Eq.by(c => BA.enumerate.map(c.run))
  }

  // core
  checkAll("Finite[Unit]", FiniteTests[Unit].finite)
  checkAll("Finite[Boolean]", FiniteTests[Boolean].finite)
  checkAll("Finite[Byte]", FiniteTests[Byte].finite)
  checkAll("Finite[Short]", FiniteTests[Short].finite)
  checkAll("Finite[Int]", FiniteTests[Int].finite)
  checkAll("Finite[Long]", FiniteTests[Long].finite)
  checkAll("Finite[Char]", FiniteTests[Char].finite)
  checkAll("Finite[(Boolean, Boolean)]", FiniteTests[(Boolean, Boolean)].finite)
  checkAll("Finite[Option[Boolean]]", FiniteTests[Option[Byte]].finite)
  checkAll("Finite[Either[Boolean, Boolean]]", FiniteTests[Either[Byte, Byte]].finite)
  checkAll("Finite[Map[Boolean, Byte]]", FiniteTests[Map[Boolean, Byte]].finite)
  checkAll("Finite[Set[Boolean]]", FiniteTests[Set[Boolean]].finite)
  checkAll("Finite[Boolean => Byte]", FiniteTests[Boolean => Byte].finite)
  checkAll("Finite[PartialFunction[Boolean, Byte]]", FiniteTests[PartialFunction[Boolean, Byte]].finite)

  // cats
  checkAll("Finite[Eval[Int]]", FiniteTests[Eval[Int]].finite)
  checkAll("Finite[Ior[Int, Int]]", FiniteTests[Ior[Int, Int]].finite)
  checkAll("Finite[NonEmptySet[Boolean]]", FiniteTests[NonEmptySet[Boolean]].finite)
  checkAll("Finite[NonEmptyMap[Boolean, Byte]]", FiniteTests[NonEmptyMap[Boolean, Byte]].finite)
  checkAll("Finite[Cont[Boolean, Boolean]]", FiniteTests[Cont[Boolean, Boolean]].finite)

  // shapeless
  sealed abstract class FooBar
  final case class Foo(x: Int) extends FooBar
  final case class Bar(x: Int, y: Int) extends FooBar
  object FooBar {
    implicit def arbitrary: Arbitrary[FooBar] =
      Arbitrary(
        Gen.oneOf(
          Arbitrary.arbitrary[Int].map(Foo),
          Arbitrary.arbitrary[(Int, Int)].map(Function.tupled(Bar))
        )
      )
    implicit def eq: Eq[FooBar] = Eq.fromUniversalEquals
  }
  checkAll("Finite[FooBar]", FiniteTests[FooBar].finite)
}
