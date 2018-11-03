package dsl

import scala.collection.immutable.Seq
import scala.meta._

trait Sorts {
  self: Utils with Constructors with Modifiers =>

  protected def Operations(obj: Term.Name, decls: Seq[Stat]) = decls.filter({
    case q"def $_: $_" => true
    case q"def $_: $_ = { ..$_ }" => true
    case _ => false
  }).map({
    case q"def $op: $signature = { ..$equs }" => Modifier(obj, op, signature, equs)
    case q"def $op: $signature" => Constructor(obj, op, signature)

  })

  protected def Implementation(obj: Term.Name, decls: Seq[Stat]) =
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
}
