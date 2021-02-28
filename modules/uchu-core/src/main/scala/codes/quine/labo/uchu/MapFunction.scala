package codes.quine.labo.uchu

/** MapFunction is a composer with `IndexOf` and `Map` with a default value.
  *
  * It behaves like as `indexOf.andThen(map.withDefaultValue(default))`.
  */
private[uchu] final case class MapFunction[A, B](indexOf: IndexOf[A], map: Map[Nat, B], default: B) extends (A => B) {
  def apply(x: A): B = {
    val k = indexOf(x)
    map.getOrElse(k, default)
  }

  override def toString(): String = s"MapFunction($indexOf, $map, $default)"
}
