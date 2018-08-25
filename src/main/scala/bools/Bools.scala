package bools

import universe.Universe

trait Bools {
  self: Universe =>

  trait BOOL {
    type bool = sort
    type sort <: Universal
    implicit val sort: sort


    //True: -> bool
    type True = True.type
    implicit object True extends Particular {
      def apply()(implicit m : True :: bool): True :: bool = m
    }

    //False: -> bool
    type False = False.type
    implicit object False extends Particular {
      def apply()(implicit m: False :: bool): False :: bool = m
    }

    //not: bool -> bool
    type not = not.type
    implicit object not extends Particular {
      def apply[X <: Particular](x : X :: bool)(implicit m: (not ∙ X) :: bool): (not ∙ X) :: bool = m
    }

    //and: bool, bool -> bool
    type and = and.type
    implicit object and extends Particular {
      def apply[X <: Particular, Y <: Particular](x: X :: bool, y: Y :: bool)
      (implicit m: ((and ∙ X) ∙ Y) :: bool): ((and ∙ X) ∙ Y) :: bool = m
    }

    //or: bool, bool -> bool
    type or = or.type
    implicit object or extends Particular {
      def apply[X <: Particular, Y <: Particular](x: X :: bool, y: Y :: bool)
      (implicit m: ((or ∙ X) ∙ Y) :: bool): ((or ∙ X) ∙ Y) :: bool = m
    }

  }
}