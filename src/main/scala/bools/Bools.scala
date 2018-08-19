package bools

import universe.Universe

trait Bools {
  self: Universe =>

  type bool = BOOL#sort

  trait BOOL {
    type sort <: Universal
    implicit val sort: sort


    //True: -> bool
    type True = True.type
    implicit object True extends Particular {
      def apply(): Morphism[True, Nothing ->: sort] = asMorphism
      implicit object asMorphism extends Morphism[True, Nothing ->: sort] {
        override def apply(x: Nothing#Rep): sort#Rep = trueImp()
      }
    }
    private[bools] def trueImp(x: Nothing#Rep): sort#Rep

    //False: -> bool
    type False = False.type
    implicit object False extends Particular {
      def apply(): Morphism[False, Nothing ->: sort] = asMorphism
      implicit object asMorphism extends Morphism[False, Nothing ->: sort] {
        override def apply(x: Nothing#Rep): sort#Rep = falseImp()
      }
    }
    private[bools] def falseImp(x: Nothing#Rep): sort#Rep

    //not: bool -> bool
    type not = not.type
    implicit object not extends Particular {
      def apply[Y <: Particular](x: Morphism[Y, Nothing ->: sort])(implicit m: Morphism[not ∙ Y, Nothing ->: sort])
      : Morphism[not ∙ Y, Nothing ->: sort] = m
      implicit object asMorphism extends Morphism[not, sort ->: sort] {
        override def apply(x: sort#Rep): sort#Rep = notImp(x)
      }
    }
    private[bools] def notImp(x: sort#Rep): sort#Rep

    //and: bool, bool -> bool
    type and = and.type
    implicit object and extends Particular {
      def apply[X <: Particular, Y <: Particular](x: Morphism[X, Nothing ->: sort], y: Morphism[Y, Nothing ->: sort])
      (implicit m: Morphism[(and ∙ X) ∙ Y, Nothing ->: sort]): Morphism[(and ∙ X) ∙ Y, Nothing ->: sort] = m
      implicit object asMorphism extends Morphism[and, sort ->: sort ->: sort] {
        override def apply(x: sort#Rep): sort#Rep => sort#Rep = andImp(x,_)
      }
    }
    private[bools] def andImp(x: sort#Rep, y: sort#Rep): sort#Rep

    //and: bool, bool -> bool
    type or = or.type
    implicit object or extends Particular {
      def apply[X <: Particular, Y <: Particular](x: Morphism[X, Nothing ->: sort], y: Morphism[Y, Nothing ->: sort])
                                                 (implicit m: Morphism[(or ∙ X) ∙ Y, Nothing ->: sort]): Morphism[(or ∙ X) ∙ Y, Nothing ->: sort] = m
      implicit object asMorphism extends Morphism[or, sort ->: sort ->: sort] {
        override def apply(x: sort#Rep): sort#Rep => sort#Rep = orImp(x,_)
      }
    }
    private[bools] def orImp(x: sort#Rep, y: sort#Rep): sort#Rep

  }
}