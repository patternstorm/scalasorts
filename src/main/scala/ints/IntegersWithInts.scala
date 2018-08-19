package ints

import universe.Universe

trait IntegersWithInts {
    self: Universe with Integers =>

    case class INTasInt(state: Int)
    object INTasInt { implicit def asInt(x: INTasInt): Int = x.state }

    object INT extends INT {
      override type sort = int.type
      implicit object int extends Universal {
        override type Rep = INTasInt
      }
      override val sort : sort = int
      override private[ints] def zeroImp(x: Nothing#Rep): INTasInt = INTasInt(0)
      override private[ints] def succImp(x: INTasInt): INTasInt = INTasInt(x.state + 1)
      override private[ints] def addImp(x: INTasInt, y: INTasInt): INTasInt = INTasInt(x.state + y.state)
    }
}
