package codes.quine.labo.uchu.laws

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import codes.quine.labo.uchu.Nat

/** SmallNat is data type for small natural number.
  *
  * It is used for specify small index.
  */
final case class SmallNat(toInt: Int) extends Ordering[SmallNat] {

  /** Converts this into Nat. */
  def toNat: Nat = Nat(toInt)

  def compare(x: SmallNat, y: SmallNat): Int = x.toInt.compare(y.toInt)
}

/** SmallNat utilities. */
object SmallNat {

  /** A SmallNat generator. */
  implicit val arbitrary: Arbitrary[SmallNat] =
    Arbitrary(Gen.chooseNum(0, 0xffff).map(SmallNat(_)))
}
