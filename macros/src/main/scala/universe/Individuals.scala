package universe

trait Individuals {

  trait Individual {
    type self <: Individual
    val symbol: String

    protected def toStringAsPart: String

    override def toString: String = symbol
  }

  trait Simple extends Individual {
    self: Singleton =>
    override type self = this.type

    override protected def toStringAsPart: String = symbol
  }

  trait Complex extends Individual {
    type self <: Complex

    override protected def toStringAsPart: String = "(" + symbol + ")"
  }

  object Individual {
    def toString[X <: Individual, Y <: Individual](x: X, y: Y, op: String): String = x.toStringAsPart + op + y.toStringAsPart
  }


}
