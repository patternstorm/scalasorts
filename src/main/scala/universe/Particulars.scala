package universe

trait Particulars {
  self: Individuals with Universals with Morphisms =>


  sealed trait Particular extends Individual {
    override type self <: Particular

    def apply[T, U <: Universal]()(implicit ev1: self :: U, ev2: Implementation[self, U, T]): T = ev2()
  }

  trait Operator extends Particular {
    self: Singleton =>
    override type self = this.type
  }

  trait Composition[X <: Particular, Y <: Particular] extends Particular {
    type left = X
    type right = Y
    override type self = X ∙ Y
  }

  type ∙[X <: Particular, Y <: Particular] = Composition[X, Y]

  trait CompisitionRules {
    implicit def asTerm2[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit ev1: X, ev2: Y, ev3: Y :: A, ev4: X :: A ->: B, ev5: A, ev6: B): X ∙ Y = new ∙[X, Y] {}
  }

  object Composition extends CompisitionRules {
    implicit def asTerm1[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit ev1: X, ev2: Y, ev3: X :: A ->: B, ev4: Y :: A, ev5: A, ev6: B): X ∙ Y = new ∙[X, Y] {}
  }

}
