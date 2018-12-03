package dsl

import scala.collection.immutable.Seq
import scala.meta._

trait Modifiers {
  self: Utils with Operations with Implementations =>

  object Modifier {

    object Morphism {

      private def term2RepTermRef(t: Term): Term.Ref = {
        val tref: Term.Ref = Term.Name(t.symbol + ".rep")
        tref
      }

      private def term2RepTermName(t: Term): Term.Name = Term.Name(t.symbol + ".rep")

      private def exprToPattern(expr: Term): Pat = expr match {
        case q"(..$args)" => p"(..${args.map(exprToPattern(_))})"
        case q"$op()" => Pat.Var.Term(term2RepTermName(op))
        case q"$op(..$args)" => p"${term2RepTermRef(op)}(..${args.map(exprToPattern(_))})"
        case t: Term.Name => Pat.Var.Term(t)
      }

      private def exprToRepExpr(op: Term.Name, rhs: Term): Term = rhs match {
        case Term.Apply(f, Seq(x)) if f === op => q"this()(${exprToRepExpr(op, x)})"
        case Term.Apply(f, Seq(x, y)) if f === op => q"this()(${exprToRepExpr(op, x)})(${exprToRepExpr(op, y)})"
        case q"$f()" => q"$f.imp()"
        case q"$f($x)" => q"$f.imp()(${exprToRepExpr(op, x)})"
        case q"$f($x,$y)" => q"$f.imp()(${exprToRepExpr(op, x)})(${exprToRepExpr(op, y)})"
        case t: Term.Name => t
      }

      private def exprToCase(op: Term.Name, lhs: Term, rhs: Term): Case = {
        Case(exprToPattern(lhs), None, exprToRepExpr(op, rhs))
      }

      private def exprToMatch1(op: Term.Name, imp: Seq[Stat]): Term.Match = {
        q"""_1 match {
            ..case ${imp.map({ case q"$lhs -> $rhs" => exprToCase(op, lhs, rhs) })}
            }
            """
      }

      private def exprToMatch2(op: Term.Name, imp: Seq[Stat]): Term.Match = {
        q"""(_1,_2) match {
            ..case ${imp.map({ case q"$lhs -> $rhs" => exprToCase(op, lhs, rhs) })}
            }
            """
      }

      private def morphism1(op: Term.Name, A: Term.Name, B: Term.Name, imp: Seq[Stat]) =
        q"""
      implicit object imp extends ($op.type :: $A.type ->: $B.type) {
        override def apply(): $A.type#rep => $B.type#rep = _1 =>  ${exprToMatch1(op, imp)}
      }"""

      private def morphism2(op: Term.Name, A: Term.Name, B: Term.Name, C: Term.Name, imp: Seq[Stat]) =
        q"""
      implicit object imp extends ($op.type :: $A.type ->: $B.type ->: $C.type) {
        override def apply(): $A.type#rep => $B.type#rep => $C.type#rep = _1 => _2 => ${exprToMatch2(op, imp)}
      }"""

      def apply(op: Term.Name, signature: Type, imp: Seq[Stat]) = signature match {
        case t"($a,$b) => $c" => morphism2(op, a, b, c, imp)
        case t"$a => $b => $c" => morphism2(op, a, b, c, imp)
        case t"$a => $b" => morphism1(op, a, b, imp)
      }
    }

    def apply(sort: Term.Name, op: Term.Name, signature: Option[Type], imp: Seq[Stat]): Defn.Object = {
      val sop: Lit.String = op.value
      q"""
      implicit object $op extends Operator {
        override type self = $op.type
        override val symbol: String = $sop
        ${Morphism(op, signature.get, imp)}
        ..${Operation(op, signature.get)}
        ${Implementation(op, signature.get)}
      }
      """
    }
  }

}
