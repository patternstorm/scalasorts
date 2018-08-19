package ints

import universe.Universe

trait Integers {
  self: Universe =>

  type int = INT#sort

  trait INT {
    type sort <: Universal
    implicit val sort: sort

    //zero: -> INT
    type zero = zero.type
    implicit object zero extends Particular {
      def apply(): zero :: Nothing ->: sort = zero.asMorphism
      implicit object asMorphism extends (zero :: Nothing ->: sort) {
        override def apply(x: Nothing#Rep): sort#Rep = zeroImp()
      }
    }
    private[ints] def zeroImp(x: Nothing#Rep): sort#Rep

    //succ: INT -> INT
    type succ = succ.type
    implicit object succ extends Particular {
      def apply[Y <: Particular](x: Y :: Nothing ->: sort)(implicit m: (succ ∙ Y) :: Nothing ->: sort) = m
      implicit object asMorphism extends Morphism[succ, sort ->: sort] {
        override def apply(x: sort#Rep): sort#Rep = succImp(x)
      }
    }
    private[ints] def succImp(x: sort#Rep): sort#Rep

    //add: INT, INT -> INT
    type add = add.type
    implicit object add extends Particular {
      def apply[X <: Particular, Y <: Particular](x: X :: Nothing ->: sort, y: Y :: Nothing ->: sort)
      (implicit m: ((add ∙ X) ∙ Y) :: Nothing ->: sort) = m
      implicit object asMorphism extends Morphism[add, sort ->: sort ->: sort] {
        override def apply(x: sort#Rep): sort#Rep => sort#Rep = addImp(x,_)
      }
    }
    private[ints] def addImp(x: sort#Rep, y: sort#Rep): sort#Rep
  }
}