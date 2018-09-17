package nats

import universe.Universe._

trait Naturals {

  type nat = nat.type

  implicit object nat extends Sort {

    override type rep = Rep[nat]

    sealed trait _nat extends rep

    case object _zero extends _nat

    case class _succ(n: rep) extends _nat


    //zero: -> nat
    type zero = zero.type

    implicit object zero extends Operator {
      def apply()(implicit m: zero :: nat): zero :: nat = m

      implicit object imp extends (zero :: nat) {
        override def apply(): rep = _zero
      }

    }

    //succ: nat -> nat
    type succ = succ.type

    implicit object succ extends Operator {
      def apply[Y <: Particular](x: Y :: nat)(implicit m: (succ ∙ Y) :: nat): (succ ∙ Y) :: nat = m

      implicit object imp extends (succ :: nat ->: nat) {
        override def apply(): rep => rep = x => _succ(x)
      }

    }

    //add: nat, nat -> nat
    type add = add.type

    implicit object add extends Operator {
      def apply[X <: Particular, Y <: Particular](x: X :: nat, y: Y :: nat)(implicit m: ((add ∙ X) ∙ Y) :: nat): ((add ∙ X) ∙ Y) :: nat = m

      implicit object imp extends (add :: nat ->: nat ->: nat) {
        override def apply(): rep => rep => rep = x => y => x match {
          case `_zero` => y
          case _succ(n) => _succ(imp()(n)(y))
        }
      }
    }

  }

}