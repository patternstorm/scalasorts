package nats

import universe.Universe

  trait Naturals {
    self: Universe =>

    trait NAT {
      type nat = sort
      type sort <: Universal
      implicit val sort: sort


      //zero: -> nat
      type zero = zero.type
      implicit object zero extends Particular {
        def apply()(implicit m: zero :: nat): zero :: nat = m
      }

      //succ: nat -> nat
      type succ = succ.type
      implicit object succ extends Particular {
        def apply[Y <: Particular](x: Y :: nat)(implicit m: (succ ∙ Y) :: nat)
        : (succ ∙ Y) :: nat = m
      }

      //add: nat, nat -> nat
      type add = add.type
      implicit object add extends Particular {
        def apply[X <: Particular, Y <: Particular](x: X :: nat, y: Y :: nat)
        (implicit m: ((add ∙ X) ∙ Y ):: nat): ((add ∙ X) ∙ Y ):: nat = m
        implicit def eq1[N <: Particular](implicit ev1: N :: sort): (add ∙ N ∙ zero) ⟿ N = new Equals[add ∙ N ∙ zero, N] {}
      }
    }
  }