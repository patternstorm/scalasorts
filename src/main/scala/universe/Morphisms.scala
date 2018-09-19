package universe

// TODO: rename to terms
trait Morphisms {
  self: Individuals with Particulars with Universals =>


  abstract class Implementation[X <: Particular, U <: Universal, T](implicit x: X, u: U, ev1: X :: U, ev2: U <-> T) {
    def apply(): T
  }

  trait ImplementationRules {
    implicit def asImp3[X <: Particular, A <: Sort, P]
    (implicit ev1: X, ev2: A, a: X :: A, ev3: A as P)
    : Implementation[X, A, P] = new Implementation[X, A, P] {
      override def apply(): P = ev3.encode(a())
    }

    implicit def asImp4[X <: Particular, Y <: Particular, A <: Sort, B <: Universal, P, Q]
    (implicit ev1: X, ev2: Y, a: Y :: A, ev4: X :: A ->: B,
     ev5: A as P, ev6: B <-> Q, f: Implementation[X, A ->: B, P => Q], ev7: A, ev8: B)
    : Implementation[(X ∙ Y), B, Q] = new Implementation[(X ∙ Y), B, Q] {
      override def apply(): Q = f()(ev5.encode(a()))
    }


    implicit def asImp5[X <: Particular, Y <: Particular, A <: Sort, B <: Universal, C <: Universal, P, Q, R]
    (implicit ev1: X, ev2: Y, a: Y :: A, ev4: X :: A ->: B ->: C,
     ev5: A as P, ev6: B <-> Q, ev7: C <-> R,
     f: Implementation[X, A ->: B ->: C, P => Q => R], ev8: A, ev9: B, ev10: C)
    : Implementation[(X ∙ Y), B ->: C, Q => R] = new Implementation[(X ∙ Y), B ->: C, Q => R] {
      override def apply(): Q => R = f()(ev5.encode(a()))
    }

  }

  object Implementation extends ImplementationRules {

    implicit def asImp1[X <: Particular, Y <: Particular, A <: Universal, B <: Universal, P, Q]
    (implicit ev1: X, ev2: Y, ev3: Y :: A, ev4: X :: A ->: B,
     ev5: A <-> P, ev6: B <-> Q,
     a: Implementation[Y, A, P], f: Implementation[X, A ->: B, P => Q], ev7: A, ev8: B)
    : Implementation[(X ∙ Y), B, Q] = new Implementation[(X ∙ Y), B, Q] {
      override def apply(): Q = f()(a())
    }


    implicit def asImp2[X <: Particular, Y <: Particular, A <: Universal, B <: Universal, C <: Universal, P, Q, R]
    (implicit ev1: X, ev2: Y, ev3: Y :: A, ev4: X :: A ->: B ->: C,
     ev5: A <-> P, ev6: B <-> Q, ev7: C <-> R,
     a: Implementation[Y, A, P], f: Implementation[X, A ->: B ->: C, P => Q => R], ev8: A, ev9: B, ev10: C)
    : Implementation[(X ∙ Y), B ->: C, Q => R] = new Implementation[(X ∙ Y), B ->: C, Q => R] {
      override def apply(): Q => R = f()(a())
    }

  }



  abstract class Morphism[X <: Particular, U <: Universal](implicit x: X, u: U) {
    def apply(): U#rep
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
