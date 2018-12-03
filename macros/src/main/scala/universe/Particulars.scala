package universe

//TODO rename Particulars to Terms?
trait Particulars {
  self: Individuals with Universals with Morphisms =>


  sealed trait Particular extends Individual {
    override type self <: Particular
  }

  trait Operator extends Particular with Simple {
    self: Singleton =>
    override type self <: Operator
  }

  trait Value extends Particular with Simple {
    self: Singleton =>
    override type self <: Value
  }

  case class Rep[A <: Representation](a: A#rep)(implicit A: A) {
    type self = self.type

    def value: A#rep = a

    case object self extends Value {
      override val symbol: String = a.toString
      override type self = self.type
      implicit val me: self.type = this

      implicit object imp extends (self.type :: A) {
        override def apply(): A#rep = a
      }

    }

  }


  trait Particular2Rep2 {
    implicit def particular2Rep2[X <: Particular, A <: Representation, U <: Sort](X: X)(implicit x: X :: U, a: A reps U, A: A): Rep[A] = {
      println("Conversion Rule 2 from " + x + " to " + A)
      Rep(a.encode(x()))
    }
  }

  object Rep extends Particular2Rep2 {
    implicit def unlift[A <: Representation](a: Rep[A]): A#rep = a.value

    implicit def lift[A <: Representation](a: A#rep)(implicit A: A): Rep[A] = Rep(a)

    implicit def particular2Rep1[X <: Particular, A <: Representation](X: X)(implicit x: X :: A, A: A): Rep[A] = {
      println("Conversion Rule 1 from " + x + " to " + A)
      Rep(x())
    }
  }


  abstract class Composition[X <: Particular, Y <: Particular](implicit X: X, Y: Y) extends Particular with Complex {
    type left = X
    type right = Y
    override type self = X ∙ Y
    override val symbol = Individual.toString(X, Y, "∙")
  }

  type ∙[X <: Particular, Y <: Particular] = Composition[X, Y]

  //  //TODO check if some rules can be eliminated
  //  trait CompositionRule4_6 {
  //    implicit def asTerm4_6[X <: Particular, Y <: Particular, A <: Representation, B <: Representation, U <: Sort, V <: Universal]
  //    (implicit X: X, Y: Y, x: X :: A ->: V, y: Y :: B, a: A reps U, b: B reps U): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule4_5 extends CompositionRule4_6 {
  //    implicit def asTerm4_5[X <: Particular, Y <: Particular, A <: Representation, B <: Representation, U <: Sort, V <: Universal]
  //    (implicit X: X, Y: Y, x: X :: A ->: V, a: A reps U, b: B reps U, y: Y :: B): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule4_4 extends CompositionRule4_5 {
  //    implicit def asTerm4_4[X <: Particular, Y <: Particular, A <: Representation, B <: Representation, U <: Sort, V <: Universal]
  //    (implicit X: X, Y: Y, y: Y :: B, x: X :: A ->: V, a: A reps U, b: B reps U): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule4_3 extends CompositionRule4_4 {
  //    implicit def asTerm4_3[X <: Particular, Y <: Particular, A <: Representation, B <: Representation, U <: Sort, V <: Universal]
  //    (implicit X: X, Y: Y, y: Y :: B, a: A reps U, b: B reps U, x: X :: A ->: V): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule4_2 extends CompositionRule4_3 {
  //    implicit def asTerm4_2[X <: Particular, Y <: Particular, A <: Representation, B <: Representation, U <: Sort, V <: Universal]
  //    (implicit X: X, Y: Y, a: A reps U, b: B reps U, x: X :: A ->: V, y: Y :: B): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule4_1 extends CompositionRule4_2 {
  //    implicit def asTerm4_1[X <: Particular, Y <: Particular, A <: Representation, B <: Representation, U <: Sort, V <: Universal]
  //    (implicit X: X, Y: Y, a: A reps U, b: B reps U, y: Y :: B, x: X :: A ->: V): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule3_6 extends CompositionRule4_1 {
  //    implicit def asTerm3_6[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, g: A reps B, f: X :: A ->: C, b: Y :: B): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule3_5 extends CompositionRule3_6 {
  //    implicit def asTerm3_5[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, g: A reps B, b: Y :: B, f: X :: A ->: C): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule3_4 extends CompositionRule3_5 {
  //    implicit def asTerm3_4[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, b: Y :: B, g: A reps B, f: X :: A ->: C): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule3_3 extends CompositionRule3_4 {
  //    implicit def asTerm3_3[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, b: Y :: B, f: X :: A ->: C, g: A reps B): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule3_2 extends CompositionRule3_3 {
  //    implicit def asTerm3_2[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, f: X :: A ->: C, b: Y :: B, g: A reps B): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule3_1 extends CompositionRule3_2 {
  //    implicit def asTerm3_1[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, f: X :: A ->: C, g: A reps B, b: Y :: B): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule2_6 extends CompositionRule3_1 {
  //    implicit def asTerm2_6[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, g: A reps B, f: X :: B ->: C, a: Y :: A): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule2_5 extends CompositionRule2_6 {
  //    implicit def asTerm2_5[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, g: A reps B, a: Y :: A, f: X :: B ->: C): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule2_4 extends CompositionRule2_5 {
  //    implicit def asTerm2_4[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, a: Y :: A, f: X :: B ->: C, g: A reps B): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule2_3 extends CompositionRule2_4 {
  //    implicit def asTerm2_3[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, a: Y :: A, g: A reps B, f: X :: B ->: C): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule2_2 extends CompositionRule2_3 {
  //    implicit def asTerm2_2[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, f: X :: B ->: C, a: Y :: A, g: A reps B): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule2_1 extends CompositionRule2_2 {
  //    implicit def asTerm2_1[X <: Particular, Y <: Particular, A <: Representation, B <: Sort, C <: Universal]
  //    (implicit x: X, y: Y, f: X :: B ->: C, g: A reps B, a: Y :: A): X ∙ Y = new ∙[X, Y] {}
  //  }
  //
  //  trait CompositionRule1_2 extends CompositionRule2_1 {
  //    implicit def asTerm1_2[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
  //    (implicit X: X, Y: Y, g: Y :: A, f: X :: A ->: B): X ∙ Y = new ∙[X, Y] {}
  //  }

  //    trait CompositionRules extends CompositionRule1_2 {
  //        implicit def asTerm1_1A[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
  //        (implicit X: X, Y: Y, f: X :: A ->: B, g: Y :: A): X ∙ Y = new ∙[X, Y] {}
  //
  //        implicit def asTerm1_1B[X <: Particular, Y <: Particular, A <: Universal, B <: Universal, C <: Universal]
  //        (implicit X: X, Y: Y, f: X :: A ->: B ->:C, g: Y :: A): X ∙ Y = new ∙[X, Y] {}
  //      }
  //    }

  //  trait CompositionRule3 {
  //    implicit def asTerm3[X <: Particular, Y <: Particular, U <: Universal, V <: Universal]
  //    (implicit X: X, Y: Y, x: X :: U ->: V, y: Y :: U): X ∙ Y = new ∙[X, Y] {
  //      override type universal = V
  //      override def apply(): V#rep = x()(y())
  //    }
  //  }
  //
  //  trait CompositionRule2 extends CompositionRule3 {
  //    implicit def asTerm2[X <: (_ <:Particular) ∙ (_ <: Particular), Y <: Particular with Simple, U <: Universal, V <: Universal]
  //    (implicit X: X, Y: Y, y: Y :: U, ev: X#universal =:= (U ->: V)): X ∙ Y = new ∙[X, Y] {
  //      override type universal = V
  //      override def apply(): V#rep = X()(y())
  //    }
  //
  //  }
  //
  //  trait CompositionRule1 extends CompositionRule2 {
  //    implicit def asTerm1[X <: Particular with Simple, Y <: Composition[_<:Particular,_<:Particular], V <: Universal]
  //    (implicit X: X, Y: Y, x: X :: Y#universal ->: V): X ∙ Y = new ∙[X, Y] {
  //      override type universal = V
  //      override def apply(): V#rep = x()(Y())
  //    }
  //  }


  object Composition {
    implicit def asTerm[X <: Particular, Y <: Particular](implicit X: X, Y: Y): X ∙ Y = new ∙[X, Y] {}
  }

}
