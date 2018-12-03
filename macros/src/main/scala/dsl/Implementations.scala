package dsl

import scala.meta._

trait Implementations {
  self: Utils =>

  //TODO check that passed Representations represent the appropriate Sorts in the operation's signature
  object Implementation {

    private def implementation0(op: Term.Name) = {
      val imp: Term.Name = identifierAsTermName(Identifier(op.value + "Imp"))
      q"""
          trait impl[A <: Representation] {
              def $op(): A#rep
              implicit def $imp(implicit s: self, A: A): (self :: A) = new Morphism[self,A] {
                override def apply(): A#rep = $op()
              }
          }
        """
    }

    private def implementation1(op: Term.Name) = {
      val imp: Term.Name = identifierAsTermName(op.value + "Imp")
      q"""
          trait impl[A <: Representation, B <: Representation] {
              def $op(a: A#rep): B#rep
              implicit def $imp(implicit s: self, A: A, B: B): (self :: A ->: B) = new Morphism[self, A ->: B]  {
                override def apply(): A#rep => B#rep = a => $op(a)
              }
          }
        """
    }

    private def implementation2(op: Term.Name) = {
      val imp: Term.Name = identifierAsTermName(op.value + "Imp")
      q"""
          trait impl[A <: Representation, B <: Representation, C <: Representation] {
              def $op(a: A#rep, b: B#rep): C#rep
              implicit def $imp(implicit s: self, A: A, B: B, C: C): (self :: A ->: B ->: C) = new Morphism[self, A ->: B ->: C]  {
                override def apply(): A#rep => B#rep => C#rep = a => b => $op(a,b)
              }
          }
        """
    }

    def apply(op: Term.Name, signature: Type) = signature match {
      case t"($a,$b) => $c" => implementation2(op)
      case t"$a => $b => $c" => implementation2(op)
      case t"$a => $b" => implementation1(op)
      case t"() => $a" => implementation0(op)
    }
  }

}
