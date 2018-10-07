package dsl

import scala.meta.{Lit, Term, Type}

trait Utils {

  private def identifierAsTermName(id: String): Term.Name = Term.Name(id)

  private def identifierAsTypeName(id: String): Type.Name = Type.Name(id)

  implicit def stringAsLiteral(string: String): Lit.String = Lit.String(string)

  implicit class TermOperations(x: Term) {
    def symbol: String = x match {
      case Term.Name(name) => Identifier(name)
      case Term.Select(p, q) => p.symbol + "." + q.symbol
    }

    def ===(y: Term): Boolean = x.symbol == y.symbol
  }

  implicit def typeArgAsTermName(t: Type.Arg): Term.Name = {
    val Type.Name(name) = t
    Term.Name(name)
  }

  implicit def typeNameAsTermName(t: Type.Name): Term.Name = {
    val Type.Name(name) = t
    Term.Name(name)
  }

  implicit def typeArgAsType(arg: Type.Arg): Type = arg match {
    case Type.Arg.Repeated(tpe) => tpe
    case Type.Arg.ByName(tpe) => tpe
    case tpe: Type => tpe
  }

  implicit def termArgAsTerm(arg: Term.Arg): Term = arg match {
    case Term.Arg.Repeated(term) => term
    case Term.Arg.Named(term, _) => term
    case term: Term => term
  }

}
