package codes.quine.labo.uchu.laws

import cats.Eval
import cats.data.Chain
import cats.data.Ior
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptyVector
import cats.kernel.Eq
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import codes.quine.labo.uchu.cats.instances._
import codes.quine.labo.uchu.shapeless.semiauto._

class UniverseLawsSuite extends munit.FunSuite with munit.DisciplineSuite {
  // core
  checkAll("Universe[Int]", UniverseTests[Int].universe)
  checkAll("Universe[BigInt]", UniverseTests[BigInt].universe)
  checkAll("Universe[String]", UniverseTests[String].universe)
  checkAll("Universe[(BigInt, BigInt)]", UniverseTests[(BigInt, BigInt)].universe)
  checkAll("Universe[Seq[Int]]", UniverseTests[Seq[Int]].universe)
  checkAll("Universe[List[Int]]", UniverseTests[List[Int]].universe)
  checkAll("Universe[Vector[Int]]", UniverseTests[Vector[Int]].universe)
  checkAll("Universe[Option[BigInt]]", UniverseTests[Option[BigInt]].universe)
  checkAll("Universe[Either[BigInt, BigInt]]", UniverseTests[Either[BigInt, BigInt]].universe)

  // cats
  checkAll("Universe[Eval[BigInt]]", UniverseTests[Eval[Int]].universe)
  checkAll("Universe[Chain[Int]]", UniverseTests[Chain[Int]].universe)
  checkAll("Universe[NonEmptyChain[Int]]", UniverseTests[NonEmptyChain[Int]].universe)
  checkAll("Universe[NonEmptySeq[Int]]", UniverseTests[NonEmptySeq[Int]].universe)
  checkAll("Universe[NonEmptyList[Int]]", UniverseTests[NonEmptyList[Int]].universe)
  checkAll("Universe[NonEmptyVector[Int]]", UniverseTests[NonEmptyVector[Int]].universe)
  checkAll("Universe[Ior[BigInt, BigInt]]", UniverseTests[Ior[BigInt, BigInt]].universe)

  // shapeless
  sealed abstract class FooBar
  final case class Foo(x: BigInt) extends FooBar
  final case class Bar(x: Int, y: Int) extends FooBar
  object FooBar {
    implicit def arbitrary: Arbitrary[FooBar] =
      Arbitrary(
        Gen.oneOf(
          Arbitrary.arbitrary[BigInt].map(Foo),
          Arbitrary.arbitrary[(Int, Int)].map(Function.tupled(Bar))
        )
      )
    implicit def eq: Eq[FooBar] = Eq.fromUniversalEquals
  }
  checkAll("Universe[FooBar]", UniverseTests[FooBar].universe)
}
