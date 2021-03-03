package codes.quine.labo.uchu.laws

import cats.kernel.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import codes.quine.labo.uchu.Finite
import codes.quine.labo.uchu.laws.UchuArbitrary._

/** FiniteTests is testing utility for Finite laws. */
trait FiniteTests[A] extends UniverseTests[A] {

  /** The underlying laws. */
  val laws: FiniteLaws[A]

  /** Returns a RuleSet of Finite laws. */
  def finite(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet = {
    new DefaultRuleSet(
      "finite",
      Some(universe),
      "max index" -> forAll(laws.finiteMaxIndex _)
    )
  }
}

/** FiniteTests utilities. */
object FiniteTests {

  /** Returns a FiniteTests instance. */
  def apply[A](implicit A: Finite[A]): FiniteTests[A] =
    new FiniteTests[A] { val laws: FiniteLaws[A] = FiniteLaws[A] }
}
