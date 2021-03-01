package codes.quine.labo.uchu

/** UniverseOrdering is an Ordering instance for a countable type.
  *
  * This uses `indexOf` for ordering, so the order is not along with the standard way.
  */
class UniverseOrdering[A](implicit val universe: Universe[A]) extends Ordering[A] {
  def compare(x: A, y: A): Int = universe.indexOf(x).compare(universe.indexOf(y))
}
