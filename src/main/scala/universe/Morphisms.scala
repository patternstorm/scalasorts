package universe

trait Morphisms extends Arrows {
  self: Individuals with Nothing =>

  trait Term[X, Y] extends Particular
  type ∙[X ,Y] = Term[X,Y]

  trait TermRules {
    implicit def asTerm2[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit ev1: X, ev2: Y, ev3: Y :: A, ev4: X :: A ->: B, ev5: A, ev6: B): X ∙ Y = new Term[X, Y] {}
  }

  object Term extends TermRules {
    implicit def asTerm1[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit ev1: X, ev2: Y, ev3: X :: A ->: B, ev4: Y :: A, ev5: A, ev6: B): X ∙ Y = new Term[X, Y] {}
  }

  abstract class Morphism[X <: Particular, U <: Universal](implicit x: X, u: U) extends (nothing#rep => U#rep)
  type ::[X <: Particular, U <: Universal] = Morphism[X,U]

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

  object Morphism /*extends MorphismRules*/  {
    implicit def asMorphism1[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit ev1: X, ev2: Y, a: Y :: A, f: X :: A ->: B, ev3: A, ev4: B)
    : (X ∙ Y) :: B = new Morphism[X ∙ Y, B] {
      override def apply(n: nothing#rep): B#rep = f()(a())
    }


    implicit def asMorphism2[X <: Particular, Y <: Particular, A <: Universal, B <: Universal, C <: Universal]
    (implicit ev1: X, ev2: Y, a: Y ::  A, f: X :: A ->: B ->: C, ev3: A, ev4: B, ev5: C)
    : (X ∙ Y) :: B ->: C = new Morphism[X ∙ Y, B ->: C] {
      override def apply(n: nothing#rep): B#rep => C#rep = f()(a())
    }

  }

}
