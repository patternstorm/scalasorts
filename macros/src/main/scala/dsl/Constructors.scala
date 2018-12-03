package dsl

import scala.meta._

trait Constructors {
  self: Utils with Operations with Representations with Implementations =>


  object Constructor {

    object Morphism {
      private def morphism0(op: Term.Name, A: Term.Name) =
        q"""
      implicit object imp extends ($op.type :: $A.type) {
        override def apply(): $A.type#rep = rep
      }"""

      private def morphism1(op: Term.Name, A: Term.Name, B: Term.Name) =
        q"""
      implicit object imp extends ($op.type :: $A.type ->: $B.type) {
        override def apply(): $A.type#rep => $B.type#rep = x => rep(x)
      }"""

      private def morphism2(op: Term.Name, A: Term.Name, B: Term.Name, C: Term.Name) =
        q"""
      implicit object imp extends ($op.type :: $A.type ->: $B.type ->: $C.type) {
        override def apply(): $A.type#rep => $B.type#rep => $C.type#rep = x => y => rep(x,y)
      }"""

      def apply(op: Term.Name, signature: Type) = signature match {
        case t"($a,$b) => $c" => morphism2(op, a, b, c)
        case t"$a => $b => $c" => morphism2(op, a, b, c)
        case t"$a => $b" => morphism1(op, a, b)
        case t"() => $a" => morphism0(op, a)
      }
    }

    def apply(sort: Term.Name, op: Term.Name, signature: Type): Defn.Object = {
      val sop: Lit.String = op.value
      q"""
      implicit object $op extends Operator {
        override type self = $op.type
        override val symbol: String = $sop
        ${Morphism(op, signature)}
        ..${Representation(op, signature)}
        ..${Operation(op, signature)}
        ${Implementation(op, signature)}
      }
      """
    }

  }

}
