package universe

trait Morphisms extends Arrows {
  self: Individuals with Nothing =>

  trait Term[X, Y] extends Particular
  type ∙[X ,Y] = Term[X,Y]

  trait TermRules {
    implicit def asTerm2[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit ev1: Y :: nothing ->: A, ev2: X :: A ->: B): X ∙ Y = new Term[X, Y] {}
  }

  object Term extends TermRules {
    implicit def asTerm1[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit ev1: X :: A ->: B, ev2: Y :: nothing ->: A): X ∙ Y = new Term[X, Y] {}
  }

  abstract class Morphism[X <: Particular, F <: Arrow](implicit x: X, a: F#Domain, b: F#Image) extends (F#Domain#rep => F#Image#rep)
  type ::[X <: Particular, F <: Arrow] = Morphism[X,F]

  trait MorphismRules {
    implicit def asMorphism3[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit x: X, y: Y, ev1: X :: A ->: B, ev2: Y :: nothing ->: A, a: A, b: B)
    : (X ∙ Y) :: nothing ->: B = new Morphism[X ∙ Y, nothing ->: B] {
      override def apply(x: nothing#rep): B#rep = ev1(ev2(x))
    }

    implicit def asMorphism4[X <: Particular, Y <: Particular, A <: Universal, B <: Universal, C <: Universal]
    (implicit x: X, y: Y, ev1: X :: A ->: B ->: C, ev2: Y :: nothing ->: A, a: A, b: B, c: C)
    : (X ∙ Y) :: B ->: C = new Morphism[X ∙ Y, B ->: C] {
      override def apply(x: B#rep): C#rep = ev2(ev1())(x)
    }
  }

  object Morphism extends MorphismRules  {
    implicit def asMorphism1[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit x: X, y: Y, ev1: Y :: nothing ->: A, ev2: X :: A ->: B, a: A, b: B)
    : (X ∙ Y) :: nothing ->: B = new Morphism[X ∙ Y, nothing ->: B] {
      override def apply(x: nothing#rep): B#rep = ev2(ev1(x))
    }


    implicit def asMorphism2[X <: Particular, Y <: Particular, A <: Universal, B <: Universal, C <: Universal]
    (implicit x: X, y: Y, ev1: Y :: nothing ->: A, ev2: X :: A ->: B ->: C, a: A, b: B, c: C)
    : (X ∙ Y) :: B ->: C = new Morphism[X ∙ Y, B ->: C] {
      override def apply(x: B#rep): C#rep = ev2(ev1())(x)
    }

  }

}
