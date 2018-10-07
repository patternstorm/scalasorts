package ints

import universe.Universe._

case class INTasInt(state: Int)

object INTasInt extends int.imp[INTasInt] {
  override def _encode(x: int.zero.rep): INTasInt = INTasInt(0)

  override def _encode(x: int.succ.rep): INTasInt = INTasInt(asImp(x._1).state + 1)

  override def _decode(x: INTasInt): int.rep = if (x.state == 0) int.zero.rep else int.succ.rep(_decode(INTasInt(x.state - 1)))

  implicit object addIntAsInt extends Implementation[int.add.type, int.sort ->: int.sort ->: int.sort, INTasInt => INTasInt => INTasInt] {
    override def apply(): INTasInt => INTasInt => INTasInt = x => y => INTasInt(x.state + y.state)
  }

}


