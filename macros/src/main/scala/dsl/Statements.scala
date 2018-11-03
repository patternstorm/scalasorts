package dsl

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import universe.Universe._
import scala.meta._


object Statements extends Utils
  with Operations with Representations
  with Constructors with Modifiers
  with Sorts {

  @compileTimeOnly("")
  class sort extends StaticAnnotation {
    inline def apply(sortDecl: Any): Any = meta {
      val Sorts = new Sorts with Utils with Operations with Representations with Constructors with Modifiers {}
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

