package nats

import universe.Universe._
import nats.NAT._


object NATasInt extends nat.impl {

  case class rep(state: Int) {
    override def toString: String = symbol + "(state=" + state + ")"
  }

  override val symbol: String = "NATasInt"
  implicit val me: NATasInt.type = this

  implicit def unlift(a: Rep[NATasInt.type]): rep = a.value

  override def _encode(x: nat.zero.rep): rep = NATasInt(0)

  override def _encode(x: nat.succ.rep): rep = NATasInt(1 + asImp(x._1).state)

  override def _decode(x: rep): nat.rep = if (x.state == 0) nat.zero.rep else nat.succ.rep(_decode(NATasInt(x.state - 1)))

  def apply(state: Int): rep = rep(state)
}

object NATasIntImp extends nat.add.impl[NATasInt.type, NATasInt.type, NATasInt.type] {
  override def add(a: NATasInt.rep, b: NATasInt.rep): NATasInt.rep = NATasInt(a.state + b.state)
}


