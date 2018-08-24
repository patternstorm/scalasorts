package nats

import universe.Universe

  trait Naturals {
    self: Universe =>

    trait NAT {
      type sort <: Universal
      implicit val sort: sort


      //zero: -> nat
      type zero = zero.type
      implicit object zero extends Particular {
        def apply(): Morphism[zero, nothing ->: sort] = asMorphism
        implicit object asMorphism extends Morphism[zero, nothing ->: sort] {
          override def apply(x: nothing#rep): sort#rep = zeroImp()
        }
      }
      private[nats] def zeroImp(x: nothing#rep): sort#rep

      //succ: nat -> nat
      type succ = succ.type
      implicit object succ extends Particular {
        def apply[Y <: Particular](x: Morphism[Y, nothing ->: sort])(implicit m: Morphism[succ ∙ Y, nothing ->: sort])
        : Morphism[succ ∙ Y, nothing ->: sort] = m
        implicit object asMorphism extends Morphism[succ, sort ->: sort] {
          override def apply(x: sort#rep): sort#rep = succImp(x)
        }
      }
      private[nats] def succImp(x: sort#rep): sort#rep

      //add: nat, nat -> nat
      type add = add.type
      implicit object add extends Particular {
        def apply[X <: Particular, Y <: Particular](x: Morphism[X, nothing ->: sort], y: Morphism[Y, nothing ->: sort])
        (implicit m: Morphism[(add ∙ X) ∙ Y, nothing ->: sort]): Morphism[(add ∙ X) ∙ Y, nothing ->: sort] = m
        implicit object asMorphism extends Morphism[add, sort ->: sort ->: sort] {
          override def apply(x: sort#rep): sort#rep => sort#rep = addImp(x,_)
        }
        implicit def eq1[N <: Particular](implicit ev1: N :: nothing ->: sort): (add ∙ N ∙ zero) ⟿ N = new Equals[add ∙ N ∙ zero, N] {}
      }
      private[nats] def addImp(x: sort#rep, y: sort#rep): sort#rep
    }
  }