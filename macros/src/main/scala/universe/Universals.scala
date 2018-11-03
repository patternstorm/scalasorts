package universe

trait Universals {
  self: Individuals =>

  trait as[U <: Sort, T] {
    implicit def encode(x: U#rep): T
    implicit def decode(x: T): U#rep
  }

  //  object as {
  //    implicit def canonicalRep[U <: Sort]: as[U, U#rep] = new as[U, U#rep] {
  //      override implicit def encode(x: U#rep): U#rep = x
  //      override implicit def decode(x: U#rep): U#rep = x
  //    }
  //  }

  trait <->[U <: Universal, T]

  trait RepRules {
    implicit def arrowRep[A <: Universal, B <: Universal, P, Q](implicit ev1: A <-> P, ev2: B <-> Q): (A ->: B) <-> (P => Q) = new <->[A ->: B, P => Q] {}
  }

  object <-> extends RepRules {
    implicit def sortRep[U <: Sort, T](implicit ev: U as T): U <-> T = new <->[U, T] {}
    implicit def tupleRep[A <: Universal, B <: Universal, P, Q](implicit ev1: A <-> P, ev2: B <-> Q): (A `,` B) <-> (P, Q) = new <->[A `,` B, (P, Q)] {}
  }


  sealed trait Universal extends Individual {
    override type self <: Universal
    type rep
  }

  type nothing = nothing.type

  implicit object nothing extends Sort {
    override val symbol = "nothing"
    override type self = nothing
    override type rep = Unit
  }

  trait Sort extends Universal with Simple {
    self: Singleton =>
    override type self = this.type
  }

  //  object Sort {
  //    implicit def from[T, U <: Sort](x: T)(implicit u: U, imp: U as T): U#rep = imp.decode(x)
  //    implicit def to[T, U <: Sort](x: U#rep)(implicit imp: U as T): T = imp.encode(x)
  //
  //    //implicit def asRep[U <: Sort, T <: U#rep]: U <-> T = new <->[U, T] {}
  //  }

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
