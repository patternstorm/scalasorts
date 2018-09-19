package nats

import universe.Universe.{∙, _}

trait Naturals {

  type nat = nat.type

  implicit object nat extends Sort {


    sealed trait rep


    //zero: -> nat
    type zero = zero.type

    implicit object zero extends Operator {
      def apply(): zero = zero

      implicit object imp extends (zero :: nat) {
        override def apply(): nat.rep = rep
      }

      case object rep extends nat.rep

    }

    //succ: nat -> nat
    type succ = succ.type

    implicit object succ extends Operator {
      def apply[Y <: Particular](x: Y)(implicit ev: Y :: nat, m: succ ∙ Y): succ ∙ Y = m

      implicit object imp extends (succ :: nat ->: nat) {
        override def apply(): nat.rep => nat.rep = x => rep(x)
      }

      case class rep(n: nat.rep) extends nat.rep

    }

    //add: nat, nat -> nat
    type add = add.type

    implicit object add extends Operator {
      def apply[X <: Particular, Y <: Particular](x: X, y: Y)(implicit ev1: X :: nat, ev2: Y :: nat, m: (add ∙ X) ∙ Y): (add ∙ X) ∙ Y = m

      implicit object imp extends (add :: nat ->: nat ->: nat) {
        override def apply(): nat.rep => nat.rep => nat.rep = x => y => x match {
          case zero.rep => y
          case succ.rep(n) => succ.rep(imp()(n)(y))
        }
      }
    }

  }

}