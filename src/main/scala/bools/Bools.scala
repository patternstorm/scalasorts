package bools

import universe.Universe._

trait Bools {

  type bool = bool.type

  implicit object bool extends Sort {

    //`true`: -> bool
    type `true` = `true`.type
    //`false`: -> bool
    type `false` = `false`.type
    //not: bool -> bool
    type not = not.type
    //and: bool, bool -> bool
    type and = and.type

    implicit object `true` extends Operator {
      def apply()(implicit m: `true` :: bool): `true` :: bool = m

      implicit object imp extends (`true` :: bool) {
        override def apply(): rep = _true
      }

    }

    //or: bool, bool -> bool
    type or = or.type

    implicit object `false` extends Operator {
      def apply()(implicit m: `false` :: bool): `false` :: bool = m

      implicit object imp extends (`false` :: bool) {
        override def apply(): rep = _false
      }

    }

    override type rep = Rep[bool]

    sealed trait Bool extends rep

    implicit object not extends Operator {
      def apply[X <: Particular](x: X :: bool)(implicit m: (not ∙ X) :: bool): (not ∙ X) :: bool = m

      implicit object imp extends (not :: bool ->: bool) {
        override def apply(): rep => rep = x => x match {
          case `_true` => _false
          case `_false` => _true
        }
      }

    }

    case object _true extends Bool

    implicit object and extends Operator {
      def apply[X <: Particular, Y <: Particular](x: X :: bool, y: Y :: bool)(implicit m: ((and ∙ X) ∙ Y) :: bool): ((and ∙ X) ∙ Y) :: bool = m

      implicit object imp extends (and :: bool ->: bool ->: bool) {
        override def apply(): rep => rep => rep = x => y => x match {
          case `_true` => y
          case `_false` => _false
        }
      }

    }

    case object _false extends Bool

    implicit object or extends Operator {
      def apply[X <: Particular, Y <: Particular](x: X :: bool, y: Y :: bool)(implicit m: ((or ∙ X) ∙ Y) :: bool): ((or ∙ X) ∙ Y) :: bool = m

      implicit object imp extends (or :: bool ->: bool ->: bool) {
        override def apply(): rep => rep => rep = x => y => x match {
          case `_true` => _true
          case `_false` => y
        }
      }

    }

  }

}