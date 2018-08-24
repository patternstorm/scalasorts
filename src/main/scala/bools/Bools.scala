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
      def apply()(implicit m : True :: nothing ->: bool) = m
    }

    //False: -> bool
    type False = False.type
    implicit object False extends Particular {
      def apply()(implicit m: False :: nothing ->: bool) = m
    }

    //not: bool -> bool
    type not = not.type
    implicit object not extends Particular {
      def apply[X <: Particular](x: X :: nothing ->: bool)(implicit m: (not ∙ X) :: nothing ->: bool) = m
    }

    //and: bool, bool -> bool
    type and = and.type
    implicit object and extends Particular {
      def apply[X <: Particular, Y <: Particular](x: X :: nothing ->: bool, y: Y :: nothing ->: bool)
      (implicit m: ((and ∙ X) ∙ Y) :: nothing ->: bool): ((and ∙ X) ∙ Y) :: nothing ->: bool = m
    }

    //or: bool, bool -> bool
    type or = or.type
    implicit object or extends Particular {
      def apply[X <: Particular, Y <: Particular](x: X :: nothing ->: bool, y: Y :: nothing ->: bool)
      (implicit m: ((or ∙ X) ∙ Y) :: nothing ->: bool) = m
    }

  }
}