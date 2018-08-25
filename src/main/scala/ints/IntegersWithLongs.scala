package ints

import universe.Universe

trait IntegersWithLongs {
    self: Universe with Integers =>

    case class INTasLong(state: Long)
    object INTasLong { implicit def asLong(x: INTasLong): Long =x.state }

    object INT extends INT {
      override type sort = int.type
      implicit object int extends Universal {
        override type rep = INTasLong
      }
      override val sort : sort = int

      implicit object zeroImp extends (zero :: int) {
        override def apply(x: nothing#rep): INTasLong = INTasLong(0)
      }

      implicit object succImp extends (succ :: int ->: int) {
        override def apply(n: nothing#rep): INTasLong => INTasLong = x => INTasLong(x.state + 1)
      }

      implicit object addImp extends (add :: int ->: int ->: int) {
        override def apply(n: nothing#rep): INTasLong => INTasLong => INTasLong = x => y => INTasLong(x.state + y.state)
      }
    }
}
