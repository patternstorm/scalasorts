package ints

import universe.Universe.{::, _}

trait Integers {

  type int = int.type

  implicit object int extends Sort {

    sealed trait rep

    //zero: -> int
    type zero = zero.type
    implicit object zero extends Operator {
      def apply()(implicit m: zero :: int): zero :: int = m

      implicit object imp extends (zero :: int) {
        override def apply(): int.rep = rep
      }

      case object rep extends int.rep

    }

    //succ: int -> int
    type succ = succ.type
    implicit object succ extends Operator {
      def apply[Y <: Particular](x: Y :: int)(implicit m: (succ ∙ Y) :: int): (succ ∙ Y) :: int = m

      case class rep(n: int.rep) extends int.rep

      implicit object imp extends (succ :: int ->: int) {
        override def apply(): int.rep => int.rep = x => succ.rep(x)
      }

    }

    //add: int, int -> int
    type add = add.type
    implicit object add extends Operator {
      def apply[X <: Particular, Y <: Particular](x: X :: int, y: Y :: int)(implicit m: ((add ∙ X) ∙ Y) :: int): ((add ∙ X) ∙ Y) :: int = m

      implicit object imp extends (add :: int ->: int ->: int) {
        override def apply(): rep => rep => rep = x => y => x match {
          case zero.rep => y
          case succ.rep(n) => succ.rep(imp()(n)(y))
        }
      }

    }

  }

}