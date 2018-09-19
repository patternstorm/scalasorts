package bools

import universe.Universe._

trait Bools {

  type bool = bool.type

  implicit object bool extends Sort {

    sealed trait rep

    //`true`: -> bool
    type `true` = `true`.type
    implicit object `true` extends Operator {
      def apply(): `true` = `true`

      implicit object imp extends (`true` :: bool) {
        override def apply(): bool.rep = rep
      }

      case object rep extends bool.rep

    }


    //`false`: -> bool
    type `false` = `false`.type
    implicit object `false` extends Operator {
      def apply(): `false` = `false`

      implicit object imp extends (`false` :: bool) {
        override def apply(): bool.rep = rep
      }

      case object rep extends bool.rep

    }


    //not: bool -> bool
    type not = not.type
    implicit object not extends Operator {
      def apply[X <: Particular](x: X)(implicit ev1: X :: bool, m: not ∙ X): not ∙ X = m

      implicit object imp extends (not :: bool ->: bool) {
        override def apply(): rep => rep = x => x match {
          case `true`.rep => `false`.rep
          case `false`.rep => `true`.rep
        }
      }

    }

    //and: bool, bool -> bool
    type and = and.type
    implicit object and extends Operator {
      def apply[X <: Particular, Y <: Particular](x: X, y: Y)(implicit ev1: X :: bool, ev2: Y :: bool, m: (and ∙ X) ∙ Y): (and ∙ X) ∙ Y = m

      implicit object imp extends (and :: bool ->: bool ->: bool) {
        override def apply(): rep => rep => rep = x => y => x match {
          case `true`.rep => y
          case `false`.rep => `false`.rep
        }
      }

    }

    //or: bool, bool -> bool
    type or = or.type
    implicit object or extends Operator {
      def apply[X <: Particular, Y <: Particular](x: X, y: Y)(implicit ev1: X :: bool, ev2: Y :: bool, m: (or ∙ X) ∙ Y): (or ∙ X) ∙ Y = m

      implicit object imp extends (or :: bool ->: bool ->: bool) {
        override def apply(): rep => rep => rep = x => y => x match {
          case `true`.rep => `true`.rep
          case `false`.rep => y
        }
      }

    }

  }

}