package nats

import universe.Universe

  trait Naturals {
    self: Universe =>

    type nat = NAT#sort

    trait NAT {
      type sort <: Universal
      implicit val sort: sort


      //zero: -> nat
      type zero = zero.type
      implicit object zero extends Particular {
        def apply(): Morphism[zero, Nothing ->: sort] = asMorphism
        implicit object asMorphism extends Morphism[zero, Nothing ->: sort] {
          override def apply(x: Nothing#Rep): sort#Rep = zeroImp()
        }
      }
      private[nats] def zeroImp(x: Nothing#Rep): sort#Rep

      //succ: nat -> nat
      type succ = succ.type
      implicit object succ extends Particular {
        def apply[Y <: Particular](x: Morphism[Y, Nothing ->: sort])(implicit m: Morphism[succ ∙ Y, Nothing ->: sort])
        : Morphism[succ ∙ Y, Nothing ->: sort] = m
        implicit object asMorphism extends Morphism[succ, sort ->: sort] {
          override def apply(x: sort#Rep): sort#Rep = succImp(x)
        }
      }
      private[nats] def succImp(x: sort#Rep): sort#Rep

      //add: nat, nat -> nat
      type add = add.type
      implicit object add extends Particular {
        def apply[X <: Particular, Y <: Particular](x: Morphism[X, Nothing ->: sort], y: Morphism[Y, Nothing ->: sort])
        (implicit m: Morphism[(add ∙ X) ∙ Y, Nothing ->: sort]): Morphism[(add ∙ X) ∙ Y, Nothing ->: sort] = m
        implicit object asMorphism extends Morphism[add, sort ->: sort ->: sort] {
          override def apply(x: sort#Rep): sort#Rep => sort#Rep = addImp(x,_)
        }
        implicit def eq1[N <: Particular](implicit ev1: N :: Nothing ->: sort): (add ∙ N ∙ zero) ⟿ N = new Equals[add ∙ N ∙ zero, N] {}
      }
      private[nats] def addImp(x: sort#Rep, y: sort#Rep): sort#Rep
    }
  }