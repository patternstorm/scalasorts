package universe

trait Morphisms {
  self: Individuals with Particulars with Universals =>


  abstract class Implementation[X <: Particular, U <: Universal, T](implicit ev1: X :: U, ev2: U <-> T) {
    def apply(): T

    override def toString: String = ev1 + " as " + apply().getClass.getName
  }

  trait ImplementationRules2 {
    implicit def asImp2[X <: Particular, A <: Sort, P](implicit x: X :: A, ev: A as P): Implementation[X, A, P] =
      new Implementation[X, A, P] {
        println("asImp2 for " + this)

        override def apply(): P = {
          println("encoding " + x)
          ev.encode(x())
        }
      }
  }

  trait ImplementationRules1 extends ImplementationRules2 {
    implicit def asImp1[X <: Particular, Y <: Particular, A <: Universal, B <: Universal, P, Q]
    (implicit ev1: X :: A ->: B, ev2: Y :: A,
     f: Implementation[X, A ->: B, P => Q],
     x: Implementation[Y, A, P],
     X: X, Y: Y, A: A, B: B,
     ev3: B <-> Q,
    )
    : Implementation[(X ∙ Y), B, Q] = new Implementation[(X ∙ Y), B, Q] {
      println("asImp1 for " + this)

      override def apply(): Q = f()(x())
    }
  }


  object Implementation extends ImplementationRules1 {
    implicit def asImp0[X <: Particular, Y <: Particular, A <: Universal, B <: Universal, C <: Universal, P, Q, R]
    (implicit ev1: X :: A ->: B ->: C, ev2: Y :: A,
     f: Implementation[X, A ->: B ->: C, P => Q => R],
     x: Implementation[Y, A, P],
     X: X, Y: Y, A: A, B: B, C: C,
     ev3: (B ->: C) <-> (Q => R),
    )
    : Implementation[(X ∙ Y), B ->: C, Q => R] = new Implementation[(X ∙ Y), B ->: C, Q => R] {
      println("asImp0 for " + this)

      override def apply(): Q => R = f()(x())
    }

  }


  abstract class Morphism[X <: Particular, U <: Universal](implicit x: X, u: U) {
    def apply(): U#rep

    override def toString: String = x + " : " + u
  }

  type ::[X <: Particular, U <: Universal] = Morphism[X, U]


  object Morphism {
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
