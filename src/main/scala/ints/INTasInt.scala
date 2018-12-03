package ints

import universe.Universe._
import ints.INT._


object INTasInt extends int.impl {

  case class rep(state: Int) {
    override def toString: String = symbol + "(state=" + state + ")"
  }

  override val symbol: String = "INTasInt"
  implicit val me: INTasInt.type = this

  implicit def unlift(a: Rep[INTasInt.type]): rep = a.value

  override def _encode(x: int.zero.rep): rep = INTasInt(0)

  override def _encode(x: int.succ.rep): rep = INTasInt(asImp(x._1).state + 1)

  override def _decode(x: rep): int.rep = if (x.state == 0) int.zero.rep else int.succ.rep(_decode(INTasInt(x.state - 1)))

  def apply(state: Int): rep = rep(state)
}

object INTasIntImp extends int.add.impl[INTasInt.type, INTasInt.type, INTasInt.type] {
  override def add(a: INTasInt.rep, b: INTasInt.rep): INTasInt.rep = INTasInt(a.state + b.state)
}


