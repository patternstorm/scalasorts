package dsl

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.collection.immutable.Seq
import scala.meta._


object Statements extends Utils with Operations with Representations with Constructors with Modifiers {


  private def Operations(obj: Term.Name, decls: Seq[Stat]) = decls.filter({
    case q"def $_: $_" => true
    case q"def $_: $_ = { ..$_ }" => true
    case _ => false
  }).map({
    case q"def $op: $signature = { ..$equs }" => Modifier(obj, op, signature, equs)
    case q"def $op: $signature" => Constructor(obj, op, signature)

  })

  private def Encoders(obj: Term.Name, decls: Seq[Stat]) = decls.filter({
    case q"def $_: $_" => true
    case _ => false
  }).map({
    case q"def $op: $_" => q"protected def _encode(x: $op.type#rep): T"
  })

  private def Cases(obj: Term.Name, decls: Seq[Stat]) = decls.filter({
    case q"def $_: $_" => true
    case _ => false
  }).map({
    case q"def $op: $_" => p"case t : $op.type#rep => _encode(t): T"
  })

  private def Implementation(obj: Term.Name, decls: Seq[Stat]) =
    q"""
    trait imp[T] {
       protected def _decode(x: T): $obj.type#rep
       implicit def asImp(x: $obj.type#rep): T = x match {
          ..case ${Cases(obj, decls)}
       }
       implicit def asRep(x: T): $obj.type#rep = _decode(x)
        implicit object imp extends ($obj.type as T) {
            override def encode(x: $obj.type#rep): T = asImp(x)
            override def decode(x: T): $obj.type#rep = asRep(x)
        }
        ..${Encoders(obj, decls)}
    }"""

  @compileTimeOnly("")
  class sort extends StaticAnnotation {
    inline def apply(sortDecl: Any): Any = meta {
      val q"..$mods trait $sort { ..$decls }" = sortDecl
      val obj: Term.Name = sort
      val sobj: Lit.String = obj.value
      val sortImp: Defn.Object =
        q"""
        object $obj extends Sort {
          type sort = $obj.type
          implicit def me: sort = this
          override val symbol = $sobj
          sealed trait rep
          ..${Operations(obj, decls)}
          ${Implementation(obj, decls)}
        }"""
      val code = sortImp.syntax
      println(code)
      sortImp
    }
  }


}
