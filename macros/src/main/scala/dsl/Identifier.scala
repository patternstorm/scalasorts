package dsl


class Identifier(val symbol: String) extends AnyVal

object Identifier {
  implicit def asString(id: Identifier): String = "`" + id.symbol + "`"

  implicit def apply(s: String): Identifier = new Identifier(s)
}



