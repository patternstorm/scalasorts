package ints

import universe.Universe._
import ints.INT._

object INTasLong extends int.impl {

  case class rep(value: Long) extends AnyVal {
    override def toString: String = symbol + "(value=" + value + ")"
  }

  override val symbol: String = "INTasLong"
  implicit val me: INTasLong.type = this

  implicit def unlift(a: Rep[INTasLong.type]): rep = a.value

  implicit def asLong(x: rep): Long = x.value

  implicit def asIntAsLong(x: Long): rep = INTasLong(x)

  override def _encode(x: int.zero.rep): rep = 0

  override def _encode(x: int.succ.rep): rep = asImp(x._1) + 1

  override def _decode(x: rep): int.rep = if (x.value == 0) int.zero.rep else int.succ.rep(_decode(x - 1))

  def apply(value: Long): rep = rep(value)
}

object INTasLongImp extends int.add.impl[INTasLong.type, INTasLong.type, INTasLong.type] {
  override def add(a: INTasLong.rep, b: INTasLong.rep): INTasLong.rep = INTasLong(a.value + b.value)
}
