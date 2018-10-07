package dsl

import scala.meta._

trait Constructors {
  self: Utils with Operations with Representations =>


  object Constructor {

    object Implementation {
      private def implementation0(op: Term.Name, A: Term.Name) =
        q"""
      implicit object imp extends ($op.type :: $A.type) {
        override def apply(): $A.type#rep = rep
      }"""

      private def implementation1(op: Term.Name, A: Term.Name, B: Term.Name) =
        q"""
      implicit object imp extends ($op.type :: $A.type ->: $B.type) {
        override def apply(): $A.type#rep => $B.type#rep = x => rep(x)
      }"""

      private def implementation2(op: Term.Name, A: Term.Name, B: Term.Name, C: Term.Name) =
        q"""
      implicit object imp extends ($op.type :: $A.type ->: $B.type ->: $C.type) {
        override def apply(): $A.type#rep => $B.type#rep => $C.type#rep = x => y => rep(x,y)
      }"""

      def apply(op: Term.Name, signature: Type) = signature match {
        case t"($a,$b) => $c" => implementation2(op, a, b, c)
        case t"$a => $b => $c" => implementation2(op, a, b, c)
        case t"$a => $b" => implementation1(op, a, b)
        case t"() => $a" => implementation0(op, a)
      }
    }

    def apply(sort: Term.Name, op: Term.Name, signature: Type): Defn.Object = {
      val sop: Lit.String = op.value
      q"""
      implicit object $op extends Operator {
        override type self = $op.type
        override val symbol: String = $sop
        ${Implementation(op, signature)}
        ..${Representation(op, signature)}
        ${Operation(op, signature)}
      }
      """
    }

  }

}
