package dsl

import scala.meta._
import scala.collection.immutable.Seq

trait Operations {
  self: Utils =>

  private def constant(op: Term.Name, A: Term.Name) = Seq(q"def apply()(implicit m: $op.type): $op.type = m")

  private def unaryOp(op: Term.Name, A: Term.Name, B: Term.Name) = Seq(
    q"def apply[Y <: Particular](x: Y)(implicit ev: Y :: $A.type, m: $op.type ∙ Y): $op.type ∙ Y = m",
    q"def apply[Y <: Representation](x: Rep[Y])(implicit ev: Y reps $A.type, m: $op.type ∙ x.self): $op.type ∙ x.self = m"
  )

  private def binaryOp(op: Term.Name, A: Term.Name, B: Term.Name, C: Term.Name) = Seq(
    q"""
      def apply[X <: Particular, Y <: Particular](x: X, y: Y)(implicit ev1: X :: $A.type, ev2: Y :: $B.type, m: $op.type ∙ X ∙ Y): $op.type ∙ X ∙ Y = m
    """,
    q"""
      def apply[X <: Representation, Y <: Particular](x: Rep[X], y: Y)(implicit ev1: X reps $A.type, ev2: Y :: $B.type, m: $op.type ∙ x.self ∙ Y): $op.type ∙ x.self ∙ Y = m
    """,
    q"""
      def apply[X <: Particular, Y <: Representation](x: X, y: Rep[Y])(implicit ev1: X :: $A.type, ev2: Y reps $B.type, m: $op.type ∙ X ∙ y.self): $op.type ∙ X ∙ y.self = m
    """,
    q"""
      def apply[X <: Representation, Y <: Representation](x: Rep[X], y: Rep[Y])(implicit ev1: X reps $A.type, ev2: Y reps $B.type, m: $op.type ∙ x.self ∙ y.self): $op.type ∙ x.self ∙ y.self = m
    """
  )

  object Operation {
    def apply(op: Term.Name, signature: Type): Seq[Stat] = signature match {
      case t"($a,$b) => $c" => binaryOp(op, a, b, c)
      case t"$a => $b => $c" => binaryOp(op, a, b, c)
      case t"$a => $b" => unaryOp(op, a, b)
      case t"() => $a" => constant(op, a)
    }
  }

}
