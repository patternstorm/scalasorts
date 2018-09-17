package ints

import universe.Universe.{::, _}

trait Integers {

  type int = int.type

  implicit object int extends Sort {

    override type rep = Rep[int]

    sealed trait _int extends rep

    case object _zero extends _int

    case class _succ(n: rep) extends _int


    //zero: -> int
    type zero = zero.type

    implicit object zero extends Operator {
      def apply()(implicit m: zero :: int): zero :: int = m

      implicit object imp extends (zero :: int) {
        override def apply(): rep = _zero
      }
    }

    //succ: int -> int
    type succ = succ.type

    implicit object succ extends Operator {
      def apply[Y <: Particular](x: Y :: int)(implicit m: (succ ∙ Y) :: int): (succ ∙ Y) :: int = m

      implicit object imp extends (succ :: int ->: int) {
        override def apply(): rep => rep = x => _succ(x)
      }
    }

    //add: int, int -> int
    type add = add.type

    implicit object add extends Operator {
      def apply[X <: Particular, Y <: Particular](x: X :: int, y: Y :: int)(implicit m: ((add ∙ X) ∙ Y) :: int): ((add ∙ X) ∙ Y) :: int = m

      implicit object imp extends (add :: int ->: int ->: int) {
        override def apply(): rep => rep => rep = x => y => x match {
          case `_zero` => y
          case _succ(n) => _succ(imp()(n)(y))
        }
      }

    }

  }

}