package universe

trait Universals {
  self: Individuals =>

  trait reps[A <: Representation, U <: Sort] {
    implicit def encode(x: U#rep): A#rep

    implicit def decode(x: A#rep): U#rep
  }


  sealed trait Universal extends Individual {
    override type self <: Universal
    type rep
  }

  type nothing = nothing.type

  implicit object nothing extends Sort {
    override val symbol = "nothing"
    override type self = nothing
    override type rep = self
  }

  trait Sort extends Universal with Simple {
    self: Singleton =>
    override type self = this.type
  }

  trait Representation extends Universal with Simple {
    self: Singleton =>
    override type self = this.type
  }

  trait Arrow extends Universal with Complex {
    type Domain <: Universal
    type Image <: Universal
    override type self <: Arrow
  }

  abstract class ->:[X <: Universal, Y <: Universal](implicit x: X, y: Y) extends Arrow {
    override type Domain = X
    override type Image = Y
    override type self = X ->: Y
    override type rep = X#rep => Y#rep
    override val symbol = x + " -> " + y //Individual.toString(x, y, " -> ")
  }

  object ->: {
    implicit def asArrow[X <: Universal, Y <: Universal](implicit x: X, y: Y): X ->: Y = new ->:[X, Y] {}
  }

  trait Tuple extends Universal with Complex {
    type left <: Universal
    type right <: Universal
    override type self <: Tuple
  }

  class `,`[A <: Universal, B <: Universal](val _1: A, val _2: B) extends Tuple {
    override type left = A
    override type right = B
    override type self = A `,` B
    override type rep = (A#rep, B#rep)
    override val symbol = _1.symbol + " , " + _2.symbol
  }

  implicit class Element[A <: Universal](a: A) {
    def `,`[B <: Universal](b: B): A `,` B = new `,`[A, B](a, b) {}
  }

  case class SBL[A <: Universal, B <: Universal](elems: A `,` B) {
    def `}`: Set[_] = ???
  }

  object Tuple {
    def `{`[A <: Universal, B <: Universal](elems: A `,` B): SBL[A, B] = SBL(elems)

    implicit def asTuple[X <: Universal, Y <: Universal](implicit x: X, y: Y): X `,` Y = new `,`[X, Y](x, y) {}
  }

}
