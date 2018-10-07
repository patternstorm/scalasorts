package universe


trait Particulars {
  self: Individuals with Universals with Morphisms =>


  sealed trait Particular extends Individual {
    override type self <: Particular
    def apply[T, U <: Universal]()(implicit ev1: self :: U, ev2: Implementation[self, U, T]): T = ev2()
  }

  trait Operator extends Particular with Simple {
    self: Singleton =>
    override type self = this.type
  }

  abstract class Composition[X <: Particular, Y <: Particular](implicit x: X, y: Y) extends Particular with Complex {
    type left = X
    type right = Y
    override type self = X ∙ Y
    override val symbol = Individual.toString(x, y, "∙")
  }

  type ∙[X <: Particular, Y <: Particular] = Composition[X, Y]

  trait CompositionRules {
    implicit def asTerm2[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit ev1: X, ev2: Y, ev3: Y :: A, ev4: X :: A ->: B, ev5: A, ev6: B): X ∙ Y = new ∙[X, Y] {}
  }

  object Composition extends CompositionRules {
    implicit def asTerm1[X <: Particular, Y <: Particular, A <: Universal, B <: Universal]
    (implicit ev1: X, ev2: Y, ev3: X :: A ->: B, ev4: Y :: A, ev5: A, ev6: B): X ∙ Y = new ∙[X, Y] {}
  }

}
