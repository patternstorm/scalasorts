package ints

import universe.Universe

trait IntegersWithInts {
    self: Universe with Integers =>

    case class INTasInt(state: Int)
    object INTasInt { implicit def asInt(x: INTasInt): Int = x.state }

    object INT extends INT {
      override type sort = int.type

      implicit object int extends Universal {
        override type rep = INTasInt
      }

      override val sort: sort = int

      implicit object zeroImp extends (zero :: int) {
        override def apply(x: nothing#rep): INTasInt = INTasInt(0)
      }

      implicit object succImp extends (succ :: int ->: int) {
        override def apply(n: nothing#rep): INTasInt => INTasInt = x => INTasInt(x.state + 1)
      }

      implicit object addImp extends (add :: int ->: int ->: int) {
        override def apply(n: nothing#rep): INTasInt => INTasInt => INTasInt = x => y => INTasInt(x.state + y.state)
      }

    }
}
