package codes.quine.labo.uchu

import scala.language.implicitConversions

import _root_.cats.kernel.Eq
import org.scalacheck.Prop
import org.scalacheck.util.Pretty

package object laws {

  /** Provides `x <-> y` syntax. */
  implicit final class IsEqArrow[A](private val lhs: A) extends AnyVal {

    /** Creates IsEq instance from two values. */
    def <->(rhs: A): IsEq[A] = IsEq(lhs, rhs)
  }

  /** Converts IsEq to scalacheck's Prop. */
  implicit def isEqToProp[A](isEq: IsEq[A])(implicit eq: Eq[A], pretty: A => Pretty): Prop = {
    val IsEq(x, y) = isEq
    if (eq.eqv(x, y)) Prop.proved
    else
      Prop.falsified :| {
        val exp = Pretty.pretty[A](y, Pretty.Params(0))
        val act = Pretty.pretty[A](x, Pretty.Params(0))
        s"""|Expected: $exp
            |Received: $act""".stripMargin
      }
  }
}
