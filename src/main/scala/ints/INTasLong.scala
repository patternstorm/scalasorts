package ints

import universe.Universe._


case class INTasLong(val value: Long) extends AnyVal

object INTasLong extends int.imp[INTasLong] {
  implicit def asLong(x: INTasLong): Long = x.value

  implicit def asIntAsLong(x: Long): INTasLong = INTasLong(x)

  override def _encode(x: int.zero.rep): INTasLong = 0

  override def _encode(x: int.succ.rep): INTasLong = asImp(x._1) + 1

  override def _decode(x: INTasLong): int.rep = if (x.value == 0) int.zero.rep else int.succ.rep(_decode(x - 1))

  implicit object addIntAsLong extends Implementation[int.add.type, int.sort ->: int.sort ->: int.sort, INTasLong => INTasLong => INTasLong] {
    override def apply(): INTasLong => INTasLong => INTasLong = x => y => x + y
  }

}
