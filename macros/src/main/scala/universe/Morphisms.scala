package universe

trait Morphisms {
  self: Individuals with Particulars with Universals =>


  abstract class Morphism[X <: Particular, U <: Universal](implicit x: X, u: U) {
    def apply(): U#rep
    override def toString: String = x + " : " + u
  }

  type ::[X <: Particular, U <: Universal] = Morphism[X, U]


  trait CompositionRules3 {

    implicit def asMorphism30[X <: Particular, Y <: Particular, A <: Representation, B <: Representation, U <: Sort, V <: Universal]
    (implicit y: Y :: B, x: X :: A ->: V, a: A reps U, b: B reps U, X: X, Y: Y, V: V)
    : (X ∙ Y) :: V = new Morphism[X ∙ Y, V] {
      println("Rule 3.0 for " + this)

      override def apply(): V#rep = x()(a.encode(b.decode(y())))
    }


    implicit def asMorphism31[X <: Particular, Y <: Particular, A <: Representation, B <: Representation, U <: Sort, V <: Universal, W <: Universal]
    (implicit y: Y :: B, x: X :: A ->: V ->: W, a: A reps U, b: B reps U, X: X, Y: Y, V: V, W: W)
    : (X ∙ Y) :: V ->: W = new Morphism[X ∙ Y, V ->: W] {
      println("Rule 3.1 for " + this)

      override def apply(): V#rep => W#rep = x()(a.encode(b.decode(y())))
    }

  }

  trait CompositionRules2 extends CompositionRules3 {

    implicit def asMorphism20[X <: Particular, Y <: Particular, A <: Representation, U <: Sort, V <: Universal]
    (implicit y: Y :: A, x: X :: U ->: V, a: A reps U, X: X, Y: Y, V: V)
    : (X ∙ Y) :: V = new Morphism[X ∙ Y, V] {
      println("Rule 2.0 for " + this)

      override def apply(): V#rep = x()(a.decode(y()))
    }


    implicit def asMorphism21[X <: Particular, Y <: Particular, A <: Representation, U <: Sort, V <: Universal, W <: Universal]
    (implicit y: Y :: A, x: X :: U ->: V ->: W, a: A reps U, X: X, Y: Y, V: V, W: W)
    : (X ∙ Y) :: V ->: W = new Morphism[X ∙ Y, V ->: W] {
      println("Rule 2.1 for " + this)

      override def apply(): V#rep => W#rep = x()(a.decode(y()))
    }

  }

  trait CompositionRules1 extends CompositionRules2 {

    implicit def asMorphism10[X <: Particular, Y <: Particular, A <: Representation, U <: Sort, V <: Universal]
    (implicit x: X :: A ->: V, a: A reps U, y: Y :: U, X: X, Y: Y, V: V)
    : (X ∙ Y) :: V = new Morphism[X ∙ Y, V] {
      println("Rule 1.0 for " + this)

      override def apply(): V#rep = x()(a.encode(y()))
    }


    implicit def asMorphism11[X <: Particular, Y <: Particular, A <: Representation, U <: Sort, V <: Universal, W <: Universal]
    (implicit y: Y :: U, x: X :: A ->: V ->: W, a: A reps U, X: X, Y: Y, V: V, W: W)
    : (X ∙ Y) :: V ->: W = new Morphism[X ∙ Y, V ->: W] {
      println("Rule 1.1 for " + this)

      override def apply(): V#rep => W#rep = x()(a.encode(y()))
    }

  }


  object Morphism extends CompositionRules1 {
    implicit def asMorphism00[X <: Particular, Y <: Particular, U <: Universal, V <: Universal]
    (implicit y: Y :: U, x: X :: U ->: V, X: X, Y: Y, V: V): (X ∙ Y) :: V = new Morphism[X ∙ Y, V] {
      println("Rule 0.0 for " + this)

      override def apply(): V#rep = x()(y())
    }

    implicit def asMorphism01[X <: Particular, Y <: Particular, U <: Universal, V <: Universal, W <: Universal]
    (implicit y: Y :: U, x: X :: U ->: V ->: W, X: X, Y: Y, V: V, W: W): (X ∙ Y) :: V ->: W = new Morphism[X ∙ Y, V ->: W] {
      println("Rule 0.1 for " + this)

      override def apply(): V#rep => W#rep = x()(y())
    }
  }


  //    implicit def asMorphism0B[X <: Particular, Y <: Particular, A <: Universal, B <: Universal, C <: Universal]
  //    (implicit X: X, Y: Y , B: B, C: C, y: Y :: A, x: X :: A ->: B ->: C)
  //    : (X ∙ Y) :: B ->: C = new Morphism[X ∙ Y, B ->: C] {
  //      println("Rule 0B for "+ this)
  //      override def apply(): B#rep => C#rep = x()(y())
  //    }

  //  }

}
