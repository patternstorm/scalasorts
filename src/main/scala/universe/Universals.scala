package universe

trait Universals {
  self: Individuals =>

  trait as[U <: Sort, T] {
    implicit def encode(x: U#rep): T

    implicit def decode(x: T): U#rep
  }

  //{val identity: UUID = UUID.randomUUID()}
  sealed trait Universal extends Individual {
    override type self <: Universal
    type rep
  }

  type nothing = nothing.type

  implicit object nothing extends Universal {
    override type self = nothing
    override type rep = Unit
  }

  trait Sort extends Universal {
    override type self = this.type
  }

  object Sort {
    implicit def from[T, U <: Sort](x: T)(implicit u: U, imp: U as T): U#rep = imp.decode(x)

    implicit def to[T, U <: Sort](x: U#rep)(implicit imp: U as T): T = imp.encode(x)
  }

  trait Arrow extends Universal {
    type Domain <: Universal
    type Image <: Universal
    override type self <: Arrow
  }

  trait ->:[X <: Universal, Y <: Universal] extends Arrow {
    override type Domain = X
    override type Image = Y
    override type self = X ->: Y
    override type rep = X#rep => Y#rep
  }

  object ->: {
    implicit def asArrow[X <: Universal, Y <: Universal]: X ->: Y = new ->:[X, Y] {}
  }

  trait Tuple extends Universal {
    type left <: Universal
    type right <: Universal
    override type self <: Tuple
  }

  class `,`[A <: Universal, B <: Universal] extends Tuple {
    type left = A;
    type right = B;
    type rep = (A, B)
  }

  implicit class Element[A <: Universal](a: A) {
    def `,`[B <: Universal](b: B): A `,` B = new `,`[A, B] {}
  }

  case class SBL[A <: Universal, B <: Universal](elems: A `,` B) {
    def `}`: Set[_] = ???
  }

  object Set {
    def `{`[A <: Universal, B <: Universal](elems: A `,` B): SBL[A, B] = SBL(elems)

    implicit def asTuple[X <: Universal, Y <: Universal]: X `,` Y = new `,`[X, Y] {}
  }

}
