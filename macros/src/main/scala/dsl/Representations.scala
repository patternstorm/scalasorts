package dsl

import scala.collection.immutable.Seq
import scala.meta._

trait Representations {
  self: Utils =>

  private def rep0(op: Term.Name, A: Term.Name): Seq[Stat] = Seq(q"type rep = rep.type", q"case object rep extends $A.type#rep")


  private def rep1(op: Term.Name, A: Term.Name, B: Term.Name) = Seq(q"case class rep(_1: $A.type#rep) extends $B.type#rep")

  private def rep2(op: Term.Name, A: Term.Name, B: Term.Name, C: Term.Name) = Seq(q"case class rep(_1: $A.type#rep, _2: $B.type#rep) extends $C.type#rep")

  object Representation {
    def apply(op: Term.Name, signature: Type): Seq[Stat] = signature match {
      case t"($a,$b) => $c" => rep2(op, a, b, c)
      case t"$a => $b => $c" => rep2(op, a, b, c)
      case t"$a => $b" => rep1(op, a, b)
      case t"() => $a" => rep0(op, a)
    }
  }

}
