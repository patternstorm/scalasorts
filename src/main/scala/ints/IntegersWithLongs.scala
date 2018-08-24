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

      override private[ints] def zeroImp(x: nothing#rep): INTasLong = INTasLong(0)
      override private[ints] def succImp(x: INTasLong): INTasLong = INTasLong(x.state + 1)
      override private[ints] def addImp(x: INTasLong, y: INTasLong): INTasLong = INTasLong(x.state + y.state)
    }
}
