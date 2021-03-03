package codes.quine.labo.uchu.laws

import cats.kernel.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

import codes.quine.labo.uchu.Universe
import codes.quine.labo.uchu.cats.CatsInstances._
import codes.quine.labo.uchu.laws.UchuArbitrary._

/** UniverseTests is testing utility for Universe laws. */
trait UniverseTests[A] extends Laws {

  /** The underlying laws. */
  val laws: UniverseLaws[A]

  /** Returns a RuleSet of Universe laws. */
  def universe(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet = {
    new DefaultRuleSet(
      "universe",
      None,
      "indexOf identity" -> forAll(laws.universeIndexOfIdentity _),
      "get identity" -> forAll(laws.universeGetIdentity _),
      "enumerate consistency" -> forAll(laws.universeEnumerateConsistency _)
    )
  }
}

/** UniverseTests utilities. */
object UniverseTests {

  /** Returns an UniverseTests instance. */
  def apply[A](implicit A: Universe[A]): UniverseTests[A] =
    new UniverseTests[A] { val laws: UniverseLaws[A] = UniverseLaws[A] }
}
