package ints

import universe.Universe

trait Integers {
  self: Universe =>

  trait INT {
    type int = sort
    type sort <: Universal
    implicit val sort: sort

    //zero: -> int
    type zero = zero.type
    implicit object zero extends Particular {
      def apply()(implicit m: zero :: int): zero :: int = m
    }

    //succ: int -> int
    type succ = succ.type
    implicit object succ extends Particular {
      def apply[Y <: Particular](x: Y :: int)(implicit m: (succ ∙ Y) :: int): (succ ∙ Y) :: int = m
    }

    //add: int, int -> int
    type add = add.type
    implicit object add extends Particular {
      def apply[X <: Particular, Y <: Particular](x: X :: int, y: Y :: int)
      (implicit m: ((add ∙ X) ∙ Y) :: int): ((add ∙ X) ∙ Y) :: int = m
    }
  }
}