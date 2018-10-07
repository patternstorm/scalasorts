package dsl

import scala.meta._

trait Operations {
  self: Utils =>

  private def constant(op: Term.Name, A: Term.Name) = q"def apply()(implicit m: $op.type): $op.type = m"

  private def unaryOp(op: Term.Name, A: Term.Name, B: Term.Name) = q"def apply[Y <: Particular](x: Y)(implicit ev: Y :: $A.type, m: $op.type ∙ Y): $op.type ∙ Y = m"

  private def binaryOp(op: Term.Name, A: Term.Name, B: Term.Name, C: Term.Name) =
    q"""
      def apply[X <: Particular, Y <: Particular](x: X, y: Y)(implicit ev1: X :: $A.type, ev2: $B.type, m: $op.type ∙ X ∙ Y): $op.type ∙ X ∙ Y = m
    """

  object Operation {
    def apply(op: Term.Name, signature: Type) = signature match {
      case t"($a,$b) => $c" => binaryOp(op, a, b, c)
      case t"$a => $b => $c" => binaryOp(op, a, b, c)
      case t"$a => $b" => unaryOp(op, a, b)
      case t"() => $a" => constant(op, a)
    }
  }

}
