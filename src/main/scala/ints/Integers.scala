package ints

import universe.Universe

trait Integers {
  self: Universe =>

  trait INT {
    type sort <: Universal
    implicit val sort: sort

    //zero: -> INT
    type zero = zero.type
    implicit object zero extends Particular {
      def apply(): zero :: nothing ->: sort = asMorphism
      implicit object asMorphism extends (zero :: nothing ->: sort) {
        override def apply(x: nothing#rep): sort#rep = zeroImp()
      }
    }
    private[ints] def zeroImp(x: nothing#rep): sort#rep

    //succ: INT -> INT
    type succ = succ.type
    implicit object succ extends Particular {
      def apply[Y <: Particular](x: Y :: nothing ->: sort)(implicit m: (succ ∙ Y) :: nothing ->: sort) = m
      implicit object asMorphism extends Morphism[succ, sort ->: sort] {
        override def apply(x: sort#rep): sort#rep = succImp(x)
      }
    }
    private[ints] def succImp(x: sort#rep): sort#rep

    //add: INT, INT -> INT
    type add = add.type
    implicit object add extends Particular {
      def apply[X <: Particular, Y <: Particular](x: X :: nothing ->: sort, y: Y :: nothing ->: sort)
      (implicit m: ((add ∙ X) ∙ Y) :: nothing ->: sort) = m
      implicit object asMorphism extends Morphism[add, sort ->: sort ->: sort] {
        override def apply(x: sort#rep): sort#rep => sort#rep = addImp(x,_)
      }
    }
    private[ints] def addImp(x: sort#rep, y: sort#rep): sort#rep
  }
}