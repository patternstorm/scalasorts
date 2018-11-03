package universe

import scala.reflect.runtime.universe._

trait Particulars {
  self: Individuals with Universals with Morphisms =>

  trait impAs[X <: Operator, T] {
    def apply(): T
  }

  implicit class ParticularOps[X <: Particular, U <: Universal](x: X)(implicit ev1: X :: U) {
    def as[P](implicit imp: Implementation[X, U, P]): P = imp()

    def apply(): U#rep = ev1()
  }

  sealed trait Particular extends Individual {
    override type self <: Particular
  }


  trait Operator extends Particular with Simple {
    self: Singleton =>
    override type self = this.type

    def apply[T, U <: Sort]()(implicit ev1: self :: U, ev2: impAs[self, T]): T = ev2()

    def apply[A, B, U <: Sort, V <: Sort](a: A)(implicit ev1: self :: U ->: V, ev2: impAs[self, (A => B)]): B = ev2()(a)

    def apply[A, B, C, U <: Sort, V <: Sort, W <: Sort](a: A, b: B)(implicit ev1: self :: U ->: V ->: W, ev2: impAs[self, (A => B => C)]): C = ev2()(a)(b)
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
