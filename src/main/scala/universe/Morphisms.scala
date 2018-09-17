package universe

// TODO: rename to terms
trait Morphisms {
  self: Individuals with Imps with Particulars with Universals =>


  abstract class Morphism[X <: Particular, U <: Universal](implicit x: X, u: U) {
    def apply(): U#rep
  }

  type ::[X <: Particular, U <: Universal] = Morphism[X, U]

  //  trait MorphismRules {
  //    implicit def asMorphism3[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
  //    (implicit x: X, y: Y, ev1: X :: A ->: B, ev2: Y :: nothing ->: A, a: A, b: B)
  //    : (X ∙ Y) :: nothing ->: B = new Morphism[X ∙ Y, nothing ->: B] {
  //      override def apply(x: nothing#rep): B#rep = ev1(ev2(x))
  //    }
  //
  //    implicit def asMorphism4[X <: Particular, Y <: Particular, A <: Universal, B <: Universal, C <: Universal]
  //    (implicit x: X, y: Y, ev1: X :: A ->: B ->: C, ev2: Y :: nothing ->: A, a: A, b: B, c: C)
  //    : (X ∙ Y) :: B ->: C = new Morphism[X ∙ Y, B ->: C] {
  //      override def apply(x: B#rep): C#rep = ev1(ev2())(x)
  //    }
  //  }

  object Morphism /*extends MorphismRules*/ {

    implicit def asMorphism1[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit ev1: X, ev2: Y, a: Y :: A, f: X :: A ->: B, ev3: A, ev4: B)
    : (X ∙ Y) :: B = new Morphism[X ∙ Y, B] {
      override def apply(): B#rep = f()(a())
    }


    implicit def asMorphism2[X <: Particular, Y <: Particular, A <: Universal, B <: Universal, C <: Universal]
    (implicit ev1: X, ev2: Y, a: Y :: A, f: X :: A ->: B ->: C, ev3: A, ev4: B, ev5: C)
    : (X ∙ Y) :: B ->: C = new Morphism[X ∙ Y, B ->: C] {
      override def apply(): B#rep => C#rep = f()(a())
    }
  }

}
